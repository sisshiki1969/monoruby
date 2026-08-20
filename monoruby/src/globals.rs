use crate::ast::{BlockInfo, Loc, Node, ParamKind, SourceInfoRef};
use std::io::Read;
use std::os::unix::ffi::OsStrExt;
use std::path::PathBuf;
use std::sync::atomic::AtomicU8;

use super::*;

mod dump;
mod error;
mod gvar;
pub(crate) mod method;
pub(crate) mod prng;
mod require;
mod store;
#[cfg(any(feature = "deopt", feature = "profile"))]
pub(crate) use dump::log_deoptimize;
#[cfg(feature = "deopt")]
pub(crate) use dump::log_identity_miss;
pub use error::*;
pub use gvar::*;
use prng::*;
pub(crate) use require::RequireLoad;
pub use require::{load_file, read_source_file};
pub use store::*;

pub static WARNING: std::sync::LazyLock<AtomicU8> = std::sync::LazyLock::new(|| AtomicU8::new(0u8));

/// `--backtrace-limit=N` (-1 = unlimited). Read by the uncaught-error
/// reporter and `Exception#full_message`.
static BACKTRACE_LIMIT: std::sync::atomic::AtomicI64 = std::sync::atomic::AtomicI64::new(-1);

/// The `--backtrace-limit` value, if one was given on the command line.
pub(crate) fn backtrace_limit() -> Option<usize> {
    match BACKTRACE_LIMIT.load(std::sync::atomic::Ordering::Relaxed) {
        n if n >= 0 => Some(n as usize),
        _ => None,
    }
}

/// The `<arch>-<os>` platform string monoruby reports as `RUBY_PLATFORM`.
///
/// This is also the name of the arch-specific subdirectory inside the
/// vendored stdlib snapshot: CRuby keys `rbconfig.rb` and
/// `rbconfig/sizeof.rb` under `<rubylibdir>/<arch>/`, and
/// `bin/vendor-ruby-stdlib` preserves that layout, so the value here must
/// match the directory that was actually vendored.
///
/// When a host CRuby was present at build time, `build.rs` bakes its exact
/// `RUBY_PLATFORM` into `MONORUBY_RUBY_PLATFORM` (crucially including the
/// macOS Darwin major version, e.g. `arm64-darwin23`). Otherwise we fall
/// back to a `cfg!`-derived default. Centralising it here keeps the
/// `$LOAD_PATH` arch-dir prepend, the `require` stub lookup, and the
/// `RUBY_PLATFORM` constant from drifting apart (previously every site
/// hard-coded `x86_64-linux`, which was wrong on aarch64 / macOS).
/// Absolute path to this build's installed library tree
/// (`~/.monoruby/v<version>`), baked in by `build.rs` as
/// `MONORUBY_INSTALL_ROOT`. It contains `lib/` (vendored stdlib + stubs),
/// `builtins/` (Ruby files loaded at startup), and `stub/` (the pinned
/// C-extension replacement root). Namespacing by version keeps concurrent
/// builds and multiple checkouts from clobbering one shared tree. The
/// host-derived runtime caches (`library_path` / `gem_path`) live at the
/// top-level `~/.monoruby/` instead, since they are version-independent.
///
/// The baked path embeds the *build* machine's `$HOME`, so a prebuilt
/// binary running under a different home (distributed release binaries,
/// containers) would point at a tree that doesn't exist there. Setting
/// `MONORUBY_INSTALL_ROOT` in the *runtime* environment overrides the
/// baked path and makes the binary relocatable: point it at wherever the
/// matching `v<version>` tree was extracted.
pub(crate) fn install_root() -> PathBuf {
    match std::env::var_os("MONORUBY_INSTALL_ROOT") {
        Some(root) if !root.is_empty() => PathBuf::from(root),
        _ => PathBuf::from(env!("MONORUBY_INSTALL_ROOT")),
    }
}

pub(crate) fn ruby_platform() -> &'static str {
    const DEFAULT_PLATFORM: &str = if cfg!(all(target_arch = "aarch64", target_os = "macos")) {
        "arm64-darwin"
    } else if cfg!(all(target_arch = "x86_64", target_os = "macos")) {
        "x86_64-darwin"
    } else if cfg!(all(target_arch = "aarch64", target_os = "linux")) {
        "aarch64-linux"
    } else {
        "x86_64-linux"
    };
    option_env!("MONORUBY_RUBY_PLATFORM")
        .map(str::trim)
        .filter(|s| !s.is_empty())
        .unwrap_or(DEFAULT_PLATFORM)
}

/// Inline generator for an ordinary builtin.
///
/// # The caller's obligation
///
/// Firing a generator at all asserts that **whatever class the receiver turns
/// out to have at run time, it resolves this call site's name to the method
/// this generator implements**. That is the caller's to guarantee, and it does
/// not depend on the parameter below: a class-set guard admits only classes it
/// re-resolved to this method, and a dispatch arm is entered only by classes
/// grouped under this target.
///
/// # What the parameter adds
///
/// `Some(class)` says the receiver's class is additionally *statically known*
/// to be exactly `class`, so the generator may emit code specialized to it.
///
/// `None` says only that the site could not name it — a dispatch arm covering
/// several classes, or the class-set guard. The resolution guarantee above
/// still holds; what is missing is which class it is. A generator whose
/// emitted code depends on that must decline (`return false`); one that reads
/// only the receiver's Value representation (`nil?`, `frozen?`, `__id__`,
/// `object_id`) ignores the parameter and keeps firing.
///
/// Keeping the receiver and argument classes on the same footing is what lets
/// one generator shape serve both, so there is no separate
/// "class-independent" registration to pick between — and no way to register
/// a generator under the wrong one.
pub(crate) type InlineGen = dyn Fn(
    &mut jitgen::AbstractState,
    &mut jitgen::asmir::AsmIr,
    &crate::jitgen::JitContext,
    &Store,
    CallSiteId,
    Option<ClassId>,
    Option<ClassId>,
) -> bool;

/// Binary-operator inline generator: the JIT-inline implementation of a
/// numeric binary operator / comparison (`Integer#+`, `Float#<`, …),
/// registered per method and fired from two contexts:
///
/// - **direct-fire** from the binop/cmp bytecode dispatchers
///   (`fire_binary_inline`), guard-free under the basic-op license
///   (`bop_deps` + eviction on redefinition) — the hot path;
/// - the ordinary `compile_method_call` inline switch for explicit sends
///   (`1.+(2)`), where the class-version and receiver guards were already
///   emitted.
///
/// The generator receives the receiver (lhs) class the site resolved on,
/// the argument (rhs) class when known, and the firing mode (value-producing
/// vs fused compare-and-branch). It emits operand guards itself only for
/// operands not already proven (`gp_ensure` / float-load discipline), so
/// both firing contexts get exactly the guards they need and no more.
pub(crate) type InlineGenBinary = dyn Fn(
    &mut jitgen::AbstractState,
    &mut jitgen::asmir::AsmIr,
    &crate::jitgen::JitContext,
    &Store,
    CallSiteId,
    ClassId,
    Option<ClassId>,
    jitgen::BinaryInlineMode,
) -> jitgen::BinaryInlineOutcome;

/// Unary-operator inline generator: the JIT-inline implementation of a
/// numeric unary operator (`Integer#-@`, `Float#-@`, `Integer#~`,
/// `Numeric#+@`), the unary mirror of [`InlineGenBinary`]. Fired guard-free
/// from the `UnOp` dispatcher (`fire_unary_inline`) under the basic-op
/// license, and from `compile_method_call` for explicit sends (`1.-@`).
///
/// There is no firing mode: a unary op has no fused compare-and-branch
/// form. The receiver class is passed because several of these methods are
/// registered on a shared ancestor (`Numeric#+@` serves Integer, Float,
/// Rational and Complex), so the generator must decline for the receivers
/// its emitted code does not cover.
pub(crate) type InlineGenUnary = dyn Fn(
    &mut jitgen::AbstractState,
    &mut jitgen::asmir::AsmIr,
    &crate::jitgen::JitContext,
    &Store,
    CallSiteId,
    ClassId,
) -> bool;

/// Universal inline generator that always declines to inline (returns
/// `false`), so the call site falls back to a normal method call. Used on
/// aarch64 for builtins whose hand-written inline asm has not been ported yet:
/// registration goes through the same path on both arches, but un-ported
/// generators register this instead of arch-specific codegen (see the
/// `inline_gen!` macro). x86 never uses it.
#[allow(dead_code)] // aarch64-only; dead on x86.
pub(crate) fn noinline_gen(
    _: &mut jitgen::AbstractState,
    _: &mut jitgen::asmir::AsmIr,
    _: &crate::jitgen::JitContext,
    _: &Store,
    _: CallSiteId,
    _: ClassId,
    _: Option<ClassId>,
) -> bool {
    false
}

pub(crate) const GLOBALS_FUNCINFO: usize =
    std::mem::offset_of!(Globals, store.functions.info) + MONOVEC_PTR;

/// Internal gvar name used by bytecodegen to save/restore `$!`
/// (`Executor::errinfo`). `$!` itself is read-only from Ruby, so the
/// generated save/restore code writes through this hooked alias instead;
/// the parentheses make the name unspellable in Ruby source.
pub(crate) const ERRINFO_INTERNAL_GVAR: &str = "$(errinfo)";

#[derive(Clone, Debug)]
pub(crate) struct ExternalContext {
    scope: Vec<(
        indexmap::IndexMap<IdentId, bytecodegen::BcLocal>,
        Option<IdentId>,
    )>,
}

impl crate::ast::LocalsContext for ExternalContext {
    fn find_lvar(&self, name: &str) -> Option<usize> {
        let id = IdentId::get_id(name);
        for (outer, scope) in self.scope.iter().enumerate() {
            if scope.0.get(&id).is_some() {
                return Some(outer + 1);
            }
        }
        None
    }
}

impl std::ops::Index<usize> for ExternalContext {
    type Output = (
        indexmap::IndexMap<IdentId, bytecodegen::BcLocal>,
        Option<IdentId>,
    );
    fn index(&self, index: usize) -> &Self::Output {
        &self.scope[index]
    }
}

impl ExternalContext {
    pub fn new() -> Self {
        Self { scope: vec![] }
    }

    pub(crate) fn len(&self) -> usize {
        self.scope.len()
    }
}

pub(crate) struct Invokers {
    pub init_stack_limit: extern "C" fn(&mut Executor) -> *const u8,
    pub method: MethodInvoker,
    pub block: BlockInvoker,
    pub block_with_self: BlockInvoker,
    pub fiber: FiberInvoker,
    pub fiber_with_self: FiberInvoker,
    /// `Fiber#transfer` first activation (no `parent_fiber` link).
    pub fiber_transfer: FiberInvoker,
    pub binding: BindingInvoker,
}

///
/// Global state.
///
pub struct Globals {
    /// main object (`self`` of toplevel).
    pub main_object: Value,
    /// function and class info.
    pub store: Store,
    /// global variables and special variables.
    pub(crate) gvars: GvarTable,
    /// The separator globals, held as plain fields rather than table
    /// entries. See [`SpecialGvars`].
    special_gvars: SpecialGvars,
    /// suppress jit compilation.
    pub no_jit: bool,
    /// suppress loading gem.
    pub no_gems: bool,
    /// library directries.
    load_path: Value,
    /// standard PRNG
    random: Box<Prng>,
    /// `$LOADED_FEATURES` / `$"` — Array of canonicalised paths
    /// (Strings) that have already been loaded. Stored as a live Ruby
    /// Value so mutations from Ruby code (`$".replace(arr)`,
    /// `$".delete(path)`, `$".clear`) propagate back to the runtime's
    /// dedup tracking. Membership checks scan the array linearly,
    /// which is fine because `require` is not on a hot path.
    pub(crate) loaded_features: Value,
    /// Features whose `require` body is currently executing, keyed by
    /// the canonical path registered in `$LOADED_FEATURES`, with the
    /// loading green thread's object id. A second thread requiring the
    /// same feature blocks until the entry clears (CRuby's per-feature
    /// load lock); the id is only compared / liveness-checked, never
    /// dereferenced, so the map needs no GC marking.
    pub(crate) loading_features: std::collections::HashMap<std::path::PathBuf, u64>,
    /// `Kernel#trace_var` hooks: per global-variable name, the commands
    /// (Procs or Strings) fired after each Ruby-level assignment.
    pub(crate) gvar_traces: std::collections::HashMap<IdentId, Vec<Value>>,
    /// The fiber scheduler installed by `Fiber.set_scheduler` (monoruby
    /// keeps one per process — fibers are M:1 on the main native thread).
    pub(crate) fiber_scheduler: Option<Value>,
    /// The exact Integer the next `srand` reports as the previous seed.
    pub(crate) random_seed_obj: Value,
    /// Every root Fiber object ever materialized (`Fiber.current` at a
    /// thread's root context). A GC triggered while an ordinary fiber runs
    /// marks only the *running* executor, so a root Fiber cached on a
    /// suspended root executor would otherwise be unreachable and swept.
    pub(crate) root_fiber_objs: Vec<Value>,
    /// cache for Symbol#name (frozen strings keyed by IdentId).
    pub(crate) symbol_names: HashMap<IdentId, Value>,
    /// Dedup table for the Ruby-3.4 "block may be ignored" warning.
    /// CRuby warns once per callee *method* normally, once per call
    /// site under `Warning[:strict_unused_block]` — the key is the
    /// callee `FuncId` (low 32 bits) with the `CallSiteId` in the high
    /// bits for the strict form. See `runtime::warn_unused_block`.
    pub(crate) unused_block_warned: std::collections::HashSet<u64>,
    /// address of invokers.
    pub(crate) invokers: Invokers,
    /// Stack of the *user* block arities for the `Enumerator`s
    /// currently being driven via `Enumerator#each`. A predicate-
    /// consuming method (e.g. `Set#divide`) driven through its
    /// no-block enumerator only sees the internal yielder proc as its
    /// block, so it reads the real arity from here instead. A stack
    /// supports nested enumerators.
    pub(crate) enum_block_arity: Vec<i64>,
    /// Per-signal trap disposition, indexed by signo (1..=32; index 0 is
    /// unused). Written by `Signal.trap` / `Kernel#trap` and consulted
    /// at the poll point (`execute_gc`) to decide whether a pending
    /// signal runs a Ruby `Proc`, is ignored, or lowers to the default
    /// exception. See doc/signal.md A7.
    pub(crate) signal_handlers: Vec<crate::codegen::signal_table::SignalDisposition>,
    /// The `Signal.trap(:EXIT, ...)` handler, if any. EXIT is a
    /// pseudo-signal: the handler is not a signal disposition but an
    /// exit hook, run *before* the `at_exit` handlers (CRuby order).
    pub(crate) exit_trap_handler: Option<Value>,
    /// `Kernel#at_exit` handler Procs, run in LIFO order at program
    /// termination. Held here as GC roots: they are reachable only from
    /// this table yet must survive until the program exits.
    pub(crate) at_exit_handlers: Vec<Value>,
    /// `ObjectSpace.define_finalizer` finalizers: `(object identity,
    /// callable)`. Keyed by the raw `Value` bits of the object (its
    /// `object_id`); the callable is invoked with that id at program
    /// termination. The callables are GC roots (see `mark`). monoruby
    /// runs finalizers only at exit, never asynchronously at GC time,
    /// which the spec explicitly permits.
    pub(crate) finalizers: Vec<(u64, Value)>,
    /// The program-argument array: the single object behind the `ARGV`
    /// constant, `$*`, and the file-name queue that `ARGF` and
    /// `Kernel#gets` consume. CRuby keeps it in a C global (`rb_argv`)
    /// rather than reading the constant back, so reassigning `ARGV`
    /// redirects neither; holding it here does the same, and lets `gets`
    /// reach the queue without a constant lookup per stream switch.
    argv: Value,
    /// The ARGF object `Kernel#gets` reads from, resolved from the
    /// `ARGF` constant on first use and then bound for the process's
    /// lifetime. CRuby binds its `argf` object at startup, so
    /// reassigning the `ARGF` constant never redirects `gets` — and
    /// binding it once keeps the constant lookup out of the `gets` hot
    /// loop (`while gets`, `-n`/`-p`).
    pub(crate) argf: Option<Value>,
    /// Memoised answer to "does the ARGF object carry a `gets` that
    /// overrides `ARGFClass#gets`?" — mspec installs such singleton
    /// stubs. Keyed on the global class version, so defining or
    /// redefining any method invalidates it.
    pub(crate) argf_gets: Option<(u32, Option<FuncId>)>,
    /// stats for deoptimization
    #[cfg(feature = "profile")]
    deopt_stats: HashMap<(FuncId, bytecodegen::BcIndex), usize>,
    #[cfg(feature = "profile")]
    jit_class_unmatched_stats: HashMap<(FuncId, ClassId), usize>,
    #[cfg(feature = "profile")]
    jit_recompile_count: HashMap<(FuncId, ClassId, RecompileReason), usize>,
    #[cfg(feature = "emit-bc")]
    dumped_bc: usize,
}

impl std::ops::Deref for Globals {
    type Target = Store;
    fn deref(&self) -> &Self::Target {
        &self.store
    }
}

impl std::ops::DerefMut for Globals {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.store
    }
}

impl alloc::GC<RValue> for Globals {
    fn mark(&self, alloc: &mut alloc::Allocator<RValue>) {
        self.main_object.mark(alloc);
        self.load_path.mark(alloc);
        self.loaded_features.mark(alloc);
        self.store.mark(alloc);
        self.gvars.mark_values(|v| v.mark(alloc));
        self.special_gvars.mark(|v| v.mark(alloc));
        self.gvar_traces
            .values()
            .flatten()
            .for_each(|v| v.mark(alloc));
        if let Some(v) = &self.fiber_scheduler {
            v.mark(alloc);
        }
        for v in &self.root_fiber_objs {
            v.mark(alloc);
        }
        self.random_seed_obj.mark(alloc);
        // The argv array and the bound ARGF object outlive any
        // reassignment of the `ARGV` / `ARGF` constants, so from then on
        // they are reachable only from here.
        self.argv.mark(alloc);
        if let Some(v) = &self.argf {
            v.mark(alloc);
        }
        self.symbol_names.values().for_each(|v| v.mark(alloc));
        // Trap handler Procs are GC roots: they are reachable only from
        // this table, yet may be invoked at any future poll point.
        for disp in &self.signal_handlers {
            if let crate::codegen::signal_table::SignalDisposition::Handler(v) = disp {
                v.mark(alloc);
            }
        }
        // `at_exit` handlers and `ObjectSpace` finalizer callables are GC
        // roots: nothing else references them, yet they must survive until
        // they run at program termination.
        for v in &self.at_exit_handlers {
            v.mark(alloc);
        }
        if let Some(v) = &self.exit_trap_handler {
            v.mark(alloc);
        }
        for (_, v) in &self.finalizers {
            v.mark(alloc);
        }
    }
}

impl Globals {
    /// Record the arity of the user block driving the current
    /// `Enumerator#each`, so a predicate-consuming method invoked
    /// through its no-block enumerator (which only sees the yielder
    /// proc) can recover the real arity.
    pub(crate) fn push_enum_block_arity(&mut self, arity: i64) {
        self.enum_block_arity.push(arity);
    }

    pub(crate) fn pop_enum_block_arity(&mut self) {
        self.enum_block_arity.pop();
    }

    pub(crate) fn current_enum_block_arity(&self) -> Option<i64> {
        self.enum_block_arity.last().copied()
    }

    /// The current trap disposition for `signo`. Out-of-range signos
    /// (defensive) report `Default`.
    pub(crate) fn signal_disposition(
        &self,
        signo: i32,
    ) -> crate::codegen::signal_table::SignalDisposition {
        self.signal_handlers
            .get(signo as usize)
            .copied()
            .unwrap_or(crate::codegen::signal_table::SignalDisposition::Default)
    }

    /// Install a new trap disposition for `signo`, returning the previous
    /// one. Caller is responsible for the matching `sigaction(2)` change.
    pub(crate) fn set_signal_disposition(
        &mut self,
        signo: i32,
        disp: crate::codegen::signal_table::SignalDisposition,
    ) -> crate::codegen::signal_table::SignalDisposition {
        std::mem::replace(&mut self.signal_handlers[signo as usize], disp)
    }
}

impl Globals {
    pub fn new(warning: u8, no_jit: bool, no_gems: bool) -> Self {
        assert_eq!(64, std::mem::size_of::<FuncInfo>());

        WARNING.store(warning, std::sync::atomic::Ordering::Relaxed);

        // Arm the optional hang watchdog (no-op unless
        // MONORUBY_HANG_WATCHDOG_SEC is set). See doc/signal.md B+.
        crate::watchdog::init();

        // Propagate the host CRuby's gem-root directories to the vendored
        // rubygems via GEM_PATH so `Gem::Specification.find_by_name` can
        // discover host-installed gems (e.g. those added by `gem install`).
        // The decoupling policy only injects each gem's *require paths*
        // into $LOAD_PATH — that lets `require` succeed but leaves the
        // rubygems specification index empty unless we also point it at
        // the host gem roots. Done here, before `init_builtins`, because
        // monoruby's `ENV` hash is materialized from `std::env::vars()`
        // during builtin init and vendored rubygems reads GEM_PATH at
        // require-time from that snapshot.
        //
        // Resolution chain (first non-empty wins):
        //   1. MONORUBY_GEM_PATH env var  — explicit override
        //   2. GEM_PATH env var           — CRuby convention, untouched
        //   3. ~/.monoruby/gem_path file  — build.rs baked
        //   4. Runtime probe              — invoke `ruby` once, cache to file
        let monoruby_dir = dirs::home_dir().unwrap().join(".monoruby");
        let gem_path_file = monoruby_dir.join("gem_path");
        let library_path_file = monoruby_dir.join("library_path");

        if let Some(p) = std::env::var_os("MONORUBY_GEM_PATH") {
            // SAFETY: `Globals::new` runs single-threaded on the main
            // thread before any worker threads are spawned.
            unsafe { std::env::set_var("GEM_PATH", p) };
        } else if std::env::var_os("GEM_PATH").is_none() {
            let cached = std::fs::read_to_string(&gem_path_file)
                .ok()
                .map(|s| s.trim().to_string())
                .filter(|s| !s.is_empty());
            let from_probe = if cached.is_some() && !crate::ruby_probe::reprobe_requested() {
                None
            } else {
                crate::ruby_probe::probe().map(|p| {
                    // Cache both files so subsequent runs skip the
                    // ~50ms `ruby` spawn. `library_path` may not have
                    // been baked either (e.g. distributed binary), so
                    // write it unconditionally when we just probed.
                    let _ = std::fs::write(&gem_path_file, &p.gem_path);
                    let _ = std::fs::write(&library_path_file, &p.library_path);
                    p.gem_path
                })
            };
            if let Some(s) = from_probe.or(cached)
                && !s.is_empty()
            {
                // SAFETY: as above — single-threaded main-thread init.
                unsafe { std::env::set_var("GEM_PATH", s) };
            }
        }

        // Derive the host CRuby's `rubylibprefix` / `ruby_api_version`
        // from GEM_PATH and stash them in env vars for `startup.rb` to
        // pick up. Done in Rust (not Ruby) so the substring slicing
        // doesn't run through monoruby's String#[] / String#split,
        // which currently set `$~` as a side effect and would leak
        // `defined?($&)` truthiness into user code (caught by the
        // `defined_hooked_special_vars` unit test).
        //
        // Expected GEM_PATH entry form: `<rubylibprefix>/gems/<X.Y.Z>`
        // (matches a standard system-style install). When no entry
        // matches this shape, the override is silently skipped — the
        // vendored `rbconfig.rb` then keeps its build-time-baked
        // values, which is the pre-existing behaviour.
        if let Ok(gp) = std::env::var("GEM_PATH") {
            for entry in gp.split(':') {
                let entry = entry.trim_end_matches('/');
                let Some(idx) = entry.rfind("/gems/") else {
                    continue;
                };
                let prefix = &entry[..idx];
                let api = &entry[idx + "/gems/".len()..];
                let mut parts = api.split('.');
                let ok = parts.clone().count() == 3
                    && parts.all(|p| !p.is_empty() && p.chars().all(|c| c.is_ascii_digit()));
                if !ok {
                    continue;
                }
                // SAFETY: as above — single-threaded main-thread init.
                unsafe {
                    std::env::set_var("MONORUBY_HOST_RUBYLIBPREFIX", prefix);
                    std::env::set_var("MONORUBY_HOST_RUBY_API_VERSION", api);
                }
                break;
            }
        }

        let main_object = Value::object(OBJECT_CLASS);

        let loaded_features =
            Value::array_from_iter(["thread.rb"].iter().map(|s| Value::string_from_str(s)));

        let invokers = CODEGEN.with(|codegen| {
            let codegen = codegen.borrow();
            Invokers {
                init_stack_limit: codegen.init_stack_limit,
                method: codegen.method_invoker,
                block: codegen.block_invoker,
                block_with_self: codegen.block_invoker_with_self,
                fiber: codegen.fiber_invoker,
                fiber_with_self: codegen.fiber_invoker_with_self,
                fiber_transfer: codegen.fiber_invoker_transfer,
                binding: codegen.binding_invoker,
            }
        });

        let mut globals = Self {
            main_object,
            store: Store::new(),
            gvars: GvarTable::new(),
            special_gvars: SpecialGvars::default(),
            no_jit,
            no_gems,
            load_path: Value::array_empty(),
            random: Box::new(Prng::new()),
            loaded_features,
            loading_features: std::collections::HashMap::default(),
            gvar_traces: std::collections::HashMap::default(),
            fiber_scheduler: None,
            random_seed_obj: Value::integer(0),
            root_fiber_objs: vec![],
            symbol_names: HashMap::default(),
            unused_block_warned: std::collections::HashSet::default(),
            // signo runs 1..=32; index by signo directly (slot 0 unused).
            // Every signal starts at the OS default ("SYSTEM_DEFAULT"),
            // except the default-installed set (SIGINT), whose runtime
            // default is "DEFAULT" (raise Interrupt). See A7.
            signal_handlers: {
                use crate::codegen::signal_table::{self, SignalDisposition};
                let mut v = vec![SignalDisposition::SystemDefault; 33];
                for &signo in signal_table::POSIX_SIGNALS {
                    v[signo as usize] = SignalDisposition::Default;
                }
                // SIGPIPE is ignored at startup (writes surface
                // Errno::EPIPE instead of killing the process, as in
                // CRuby); trap reports the never-trapped state as nil.
                v[libc::SIGPIPE as usize] = SignalDisposition::Ignore { from_nil: true };
                v
            },
            invokers,
            enum_block_arity: Vec::new(),
            exit_trap_handler: None,
            at_exit_handlers: Vec::new(),
            finalizers: Vec::new(),
            argv: Value::array_empty(),
            argf: None,
            argf_gets: None,
            #[cfg(feature = "profile")]
            deopt_stats: HashMap::default(),
            #[cfg(feature = "profile")]
            jit_class_unmatched_stats: HashMap::default(),
            #[cfg(feature = "profile")]
            jit_recompile_count: HashMap::default(),
            #[cfg(feature = "emit-bc")]
            dumped_bc: 1,
        };

        let mut object_class =
            globals.define_builtin_class("Object", OBJECT_CLASS, None, OBJECT_CLASS, ObjTy::OBJECT);
        let basic_object = globals.define_builtin_class(
            "BasicObject",
            BASIC_OBJECT_CLASS,
            None,
            OBJECT_CLASS,
            ObjTy::OBJECT,
        );
        object_class.set_superclass(Some(basic_object));
        // Seed the root of the alloc_func inheritance chain with the default
        // generic allocator. `generate_class_obj` copies this field from the
        // superclass at class-creation time, so every subsequent class picks
        // it up automatically unless a builtin overrides it.
        globals.store[BASIC_OBJECT_CLASS].set_alloc_func(default_alloc_func);
        globals.store[OBJECT_CLASS].set_alloc_func(default_alloc_func);
        globals.set_constant(
            BASIC_OBJECT_CLASS,
            IdentId::get_id("BasicObject"),
            basic_object.get(),
        );
        assert_eq!(
            ENUM_YIELDER_FUNCID,
            globals.define_builtin_func(OBJECT_CLASS, "", enum_yielder, 0)
        );
        assert_eq!(
            YIELDER_FUNCID,
            globals.define_builtin_func_rest(OBJECT_CLASS, "", yielder)
        );
        assert_eq!(
            SYMBOL_TO_PROC_BODY_FUNCID,
            globals.define_builtin_func_with(OBJECT_CLASS, "", symbol_to_proc_body, 1, 1, true)
        );
        assert_eq!(
            METHOD_TO_PROC_BODY_FUNCID,
            globals.define_builtin_func_rest(OBJECT_CLASS, "", method_to_proc_body)
        );
        assert_eq!(
            PROC_CURRY_BODY_FUNCID,
            globals.define_builtin_func_rest(
                OBJECT_CLASS,
                "",
                crate::builtins::proc::proc_curry_body
            )
        );
        assert_eq!(
            WITH_INDEX_ADAPTER_FUNCID,
            globals.define_builtin_func_rest(OBJECT_CLASS, "", with_index_adapter)
        );
        assert_eq!(
            WITH_OBJECT_ADAPTER_FUNCID,
            globals.define_builtin_func_rest(OBJECT_CLASS, "", with_object_adapter)
        );
        globals.random_init(None);
        gvar::init_builtin_gvars(&mut globals);
        crate::builtins::init_builtins(&mut globals);
        // `ARGV` exists from the start — empty until the CLI fills it
        // in — and names the same array as `$*` and the ARGF queue.
        globals.set_argv(globals.argv());
        globals
            .store
            .set_ivar(main_object, IdentId::_NAME, Value::string_from_str("main"))
            .unwrap();
        // CRuby defines main's `to_s`/`inspect` as *singleton* methods
        // (`method(:to_s).owner == main.singleton_class`), and copies of
        // main (`clone`, `Kernel#load` with `wrap`) keep them through the
        // copied singleton class.
        globals.define_builtin_singleton_func(
            main_object,
            "to_s",
            crate::builtins::kernel::main_to_s,
            0,
        );
        globals.define_builtin_singleton_func(
            main_object,
            "inspect",
            crate::builtins::kernel::main_to_s,
            0,
        );

        // Load library path. Resolution chain mirrors the GEM_PATH
        // chain above; the runtime probe that may have just populated
        // `library_path_file` (via the GEM_PATH block) is the same
        // ruby invocation, so by the time we get here the cache file
        // is either already populated or unprobeable.
        let path_list = if let Some(p) = std::env::var_os("MONORUBY_LOAD_PATH") {
            p.to_string_lossy().into_owned()
        } else {
            match std::fs::read_to_string(&library_path_file) {
                Ok(s) => s,
                Err(_) => {
                    eprintln!(
                        "Warning: failed to read library path file: {:?}. Ruby may not be installed.",
                        library_path_file
                    );
                    String::new()
                }
            }
        };
        // prepend monoruby-specific lib directory so it can override CRuby stdlib files.
        // CRuby ships some stdlib files (e.g. `rbconfig/sizeof.rb`) under a
        // platform-specific subdirectory and adds that subdir to `$LOAD_PATH`
        // alongside the generic one; mirror that here so a bare
        // `require "rbconfig/sizeof"` resolves to the arch-specific stub
        // (`ruby_platform()` — the same string vendored as the subdir name).
        let monoruby_lib = install_root().join("lib");
        let monoruby_arch_lib = monoruby_lib.join(ruby_platform());
        globals.extend_load_path(
            [&monoruby_lib, &monoruby_arch_lib]
                .into_iter()
                .map(|p| p.to_string_lossy().into_owned()),
        );
        // Skip blank lines (the cache file ends with a newline): an
        // empty `$LOAD_PATH` entry would make bare `require`s resolve
        // against the CWD, which CRuby forbids for security.
        let list: Vec<_> = path_list
            .split('\n')
            .filter(|s| !s.is_empty())
            .map(|s| s.to_string())
            .collect();
        globals.extend_load_path(list.iter().cloned());

        // set constants
        let pcg_name = env!("CARGO_PKG_NAME");
        let pcg_version = env!("CARGO_PKG_VERSION");

        // The reported Ruby language level is baked in at compile time by
        // build.rs (env `MONORUBY_RUBY_VERSION`, read from the vendored
        // stdlib snapshot's `.ruby-version` pin so it always matches the
        // stdlib monoruby actually ships, host-independently). The fallback
        // mirrors that pin for the rare case the marker is missing at build
        // time, so the interpreter runs with zero runtime Ruby dependency
        // instead of panicking on a missing cache file.
        const DEFAULT_RUBY_VERSION: &str = "4.0.2";
        let ruby_version = option_env!("MONORUBY_RUBY_VERSION")
            .unwrap_or(DEFAULT_RUBY_VERSION)
            .trim()
            .to_string();

        // Build all the top-level RUBY_* String constants up-front.
        // CRuby exposes these as frozen Strings; ruby/spec
        // (`core/builtin_constants`) asserts both `is_a?(String)` and
        // `frozen?` for every one of them.
        //
        // RUBY_PLATFORM mirrors CRuby's `<arch>-<os>` convention so Ruby
        // code that branches on it (e.g. fiddle test fixtures picking
        // libc.so.6 vs libSystem.B.dylib) sees the right thing on the
        // host monoruby is actually running on. When a host CRuby was
        // present at build time, build.rs bakes its exact RUBY_PLATFORM
        // into MONORUBY_RUBY_PLATFORM — crucially including the macOS
        // Darwin major version (e.g. `arm64-darwin23`), which startup.rb
        // feeds into `RbConfig::CONFIG["arch"]` so rubygems finds each
        // gem's built C-extension directory. The cfg!-derived default is
        // only the fallback when no Ruby was available at build time.
        let platform = ruby_platform();
        let mut ruby_description = Value::string(format!("{pcg_name} {pcg_version} [{platform}]"));
        let mut ruby_engine = Value::string_from_str("ruby");
        let mut ruby_version_val = Value::string_from_str(&ruby_version);
        let mut ruby_engine_version = Value::string_from_str(&ruby_version);
        let mut ruby_platform = Value::string_from_str(platform);
        let mut ruby_copyright =
            Value::string_from_str("ruby - Copyright (C) 1993-2025 Yukihiro Matsumoto");
        let mut ruby_release_date = Value::string_from_str("2025-12-25");
        let mut ruby_revision = Value::string_from_str("monoruby");
        let ruby_patchlevel = Value::integer(0);

        for v in [
            &mut ruby_description,
            &mut ruby_engine,
            &mut ruby_version_val,
            &mut ruby_engine_version,
            &mut ruby_platform,
            &mut ruby_copyright,
            &mut ruby_release_date,
            &mut ruby_revision,
        ] {
            v.set_frozen();
        }

        globals.set_constant_by_str(OBJECT_CLASS, "RUBY_DESCRIPTION", ruby_description);
        globals.set_constant_by_str(OBJECT_CLASS, "RUBY_ENGINE", ruby_engine);
        globals.set_constant_by_str(OBJECT_CLASS, "RUBY_VERSION", ruby_version_val);
        globals.set_constant_by_str(OBJECT_CLASS, "RUBY_ENGINE_VERSION", ruby_engine_version);
        globals.set_constant_by_str(OBJECT_CLASS, "RUBY_PLATFORM", ruby_platform);
        globals.set_constant_by_str(OBJECT_CLASS, "RUBY_COPYRIGHT", ruby_copyright);
        globals.set_constant_by_str(OBJECT_CLASS, "RUBY_RELEASE_DATE", ruby_release_date);
        globals.set_constant_by_str(OBJECT_CLASS, "RUBY_REVISION", ruby_revision);
        globals.set_constant_by_str(OBJECT_CLASS, "RUBY_PATCHLEVEL", ruby_patchlevel);

        // Ruby 4.0+ exposes a `Ruby` module mirroring every RUBY_*
        // constant under a non-prefixed name. The mirror constants
        // must share *object identity* with the top-level ones — the
        // spec uses `equal?`. (This was introduced in 4.0; for older
        // Rubies the module simply doesn't exist.)
        let ruby_mod = globals.define_toplevel_module("Ruby");
        let ruby_mod_id = ruby_mod.id();
        globals.set_constant_by_str(ruby_mod_id, "VERSION", ruby_version_val);
        globals.set_constant_by_str(ruby_mod_id, "PATCHLEVEL", ruby_patchlevel);
        globals.set_constant_by_str(ruby_mod_id, "DESCRIPTION", ruby_description);
        globals.set_constant_by_str(ruby_mod_id, "ENGINE", ruby_engine);
        globals.set_constant_by_str(ruby_mod_id, "ENGINE_VERSION", ruby_engine_version);
        globals.set_constant_by_str(ruby_mod_id, "PLATFORM", ruby_platform);
        globals.set_constant_by_str(ruby_mod_id, "COPYRIGHT", ruby_copyright);
        globals.set_constant_by_str(ruby_mod_id, "RELEASE_DATE", ruby_release_date);
        globals.set_constant_by_str(ruby_mod_id, "REVISION", ruby_revision);

        globals
    }

    pub fn new_test() -> Self {
        Globals::new(1, false, true)
    }

    pub fn locals_len(&self, func_id: FuncId) -> usize {
        match self.store[func_id].kind {
            FuncKind::ISeq(info) => self.store[info].locals.len(),
            _ => 0,
        }
    }

    pub fn run(&mut self, code: impl Into<Vec<u8>>, path: &std::path::Path) -> Result<Value> {
        self.run_with_requires(&[], code, path)
    }

    ///
    /// Run *code* after `require`-ing each library in *requires* (the
    /// command-line `-r` option).
    ///
    /// The libraries are required inside the same `Executor` that runs
    /// *code*, so any `at_exit` handlers they register are drained exactly
    /// once — after *code* finishes — rather than at the end of a separate
    /// run. *code* itself is left untouched (nothing is prepended), so a
    /// source-encoding magic comment stays on its first line.
    ///
    pub fn run_with_requires(
        &mut self,
        requires: &[String],
        code: impl Into<Vec<u8>>,
        path: &std::path::Path,
    ) -> Result<Value> {
        self.run_with_prelude(requires, "", code, path)
    }

    ///
    /// Like [`Globals::run_with_requires`], but first evaluates
    /// *prelude* (Ruby source synthesized from command-line switches:
    /// `Warning[:...]` category overrides, `-E`/`-U`/`-K` encoding
    /// defaults, `-0`/`-l`/`-F` separator globals, …) inside the same
    /// `Executor`, before the `-r` requires and the script itself.
    /// An empty *prelude* is skipped entirely.
    ///
    pub fn run_with_prelude(
        &mut self,
        requires: &[String],
        prelude: &str,
        code: impl Into<Vec<u8>>,
        path: &std::path::Path,
    ) -> Result<Value> {
        let code: Vec<u8> = code.into();
        let program_name = path.to_string_lossy().to_string();
        let mut executor = Executor::init(self, &program_name)?;
        executor.init_stack_limit(self);
        let res = (|| {
            if !prelude.is_empty() {
                executor.exec_script(
                    self,
                    prelude.as_bytes().to_vec(),
                    std::path::Path::new("<internal:cli>"),
                )?;
            }
            for lib in requires {
                executor.require(self, std::path::Path::new(lib), false)?;
            }
            executor.exec_main_script(self, code, path)
        })();
        // Run `at_exit` handlers and `ObjectSpace` finalizers before the
        // process leaves. This must happen even when `res` is a
        // `SystemExit` (raised by `Kernel#exit`) or an uncaught
        // exception — CRuby runs both on any normal-ish termination.
        // `Kernel#exit!` / `Process.exit!` bypass this by calling
        // `std::process::exit` directly and never returning here.
        //
        // The script's uncaught exception is visible to the handlers as
        // `$!` (CRuby: at_exit blocks inspect the dying exception). The
        // unwind-time `$!` is restored afterwards — the top-level report
        // re-materializes the exception and derives its implicit cause
        // from it, so the handler-time materialization must not replace it.
        let unwind_errinfo = executor.errinfo();
        if let Err(err) = &res
            && !matches!(err.kind(), MonorubyErrKind::SystemExit(_))
        {
            executor.set_error(err.clone());
            let err_val = executor.take_ex_obj(self);
            executor.set_errinfo(err_val);
        }
        let handler_status = executor.run_exit_handlers(self);
        executor.set_errinfo(unwind_errinfo);
        // CRuby kills the remaining threads *after* the `at_exit`
        // handlers and *before* the uncaught-exception report: each
        // runs the ensure clauses of its current fiber chain (never of
        // suspended fibers) on the way out.
        crate::scheduler::terminate_all(&mut executor, self);
        crate::rvalue::io::flush_std_streams();
        #[cfg(any(feature = "profile", feature = "jit-log"))]
        self.show_stats();
        #[cfg(feature = "gc-log")]
        {
            alloc::ALLOC.with(|alloc| {
                let alloc = alloc.borrow();
                eprintln!("garbage collector profile:");
                eprintln!("total allocated objects: {}", alloc.total_allocated());
                eprintln!("total gc executed count: {}", alloc.total_gc_counter());
                eprintln!("  minor gc count:        {}", alloc.minor_gc_count());
                eprintln!("  major gc count:        {}", alloc.major_gc_count());
                eprintln!("old-gen objects:         {}", alloc.old_count_popcount());
                eprintln!("live objects (last gc):  {}", alloc.live_count());
                eprintln!("free list (last gc):     {}", alloc.free_count());
                eprintln!("active pages:            {}", alloc.pages_len());
            });
        }
        // Materialize an uncaught exception into its Ruby object while
        // the executor is still alive: this runs the implicit `$!`
        // cause chaining and hangs the object off the returned error
        // (`original`), so the top-level report can show a custom
        // string backtrace and walk the `#cause` chain. Control-flow
        // pseudo-errors and `SystemExit` keep their dedicated handling
        // in `main::handle_error`. Nothing Ruby-level runs after this
        // point, so the object can't be collected out from under the
        // report.
        let res = match res {
            Err(err)
                if !matches!(
                    err.kind(),
                    MonorubyErrKind::SystemExit(_)
                        | MonorubyErrKind::MethodReturn(..)
                        | MonorubyErrKind::BlockBreak(..)
                ) =>
            {
                executor.set_error(err);
                let obj = executor.take_ex_obj(self);
                let err =
                    MonorubyErr::new_from_exception(obj.is_exception().unwrap()).with_original(obj);
                Err(err)
            }
            other => other,
        };
        // An exit status chosen by the handlers themselves (a
        // `SystemExit` raised in one, or status 1 after an uncaught
        // handler exception) overrides the script's own. The script's
        // non-SystemExit error is still reported first, as CRuby
        // prints both.
        match handler_status {
            Some(status) => {
                if let Err(err) = res
                    && !matches!(err.kind(), MonorubyErrKind::SystemExit(_))
                {
                    err.show_error_message_and_all_loc(&self.store);
                }
                Err(MonorubyErr::new(
                    MonorubyErrKind::SystemExit(status as i64),
                    "exit",
                ))
            }
            None => res,
        }
    }

    pub(crate) fn compile_script_eval(
        &mut self,
        code: Vec<u8>,
        path: impl Into<PathBuf>,
        caller_cfp: Cfp,
        receiver_class: Option<ClassId>,
        lineno: i64,
        src_encoding: Option<String>,
    ) -> Result<FuncId> {
        let line_offset = lineno - 1;
        // Eval'd code compiled against a live frame can reach raw
        // parameter slots anywhere on the caller's outer chain (e.g. a
        // zsuper reading the mother method's rest/kwrest), so no lazy
        // `(...)`-forwarding marker may be left pending in any frame it
        // could observe.
        crate::codegen::runtime::materialize_lazy_forwarding(self, caller_cfp);
        // Walk the CFP chain to find the nearest *iseq* (Ruby) frame.
        // `Module#class_eval` / `instance_eval` / `Kernel#eval` may be
        // invoked indirectly through builtin frames (mspec, helpers
        // written in Rust, …), so the immediate caller often isn't a
        // Ruby method itself. CRuby's `rb_eval_string` likewise looks
        // up the nearest cref/binding rather than failing at the first
        // C frame.
        //
        // Side-effect note: only the *outer scope* (lexical
        // ancestors + visible locals) is taken from this iseq;
        // arguments, self, and the lexical-class override (`class_eval`
        // pushing the receiver as the eval cref) come from the caller
        // we were originally given.
        let outer = {
            let mut frame = Some(caller_cfp);
            loop {
                match frame {
                    Some(cfp) => match self.store[cfp.lfp().func_id()].is_iseq() {
                        Some(iseq) => break Some(iseq),
                        None => frame = cfp.prev(),
                    },
                    None => break None,
                }
            }
        };
        let outer = match outer {
            Some(iseq) => iseq,
            None => {
                return Err(MonorubyErr::runtimeerr(
                    "eval requires a Ruby method context",
                ));
            }
        };
        let external_context = self.store.scoped_locals(outer);

        match crate::parser::parse_program_eval(
            code,
            path,
            Some(&external_context),
            line_offset,
            src_encoding,
        ) {
            Ok(result) => {
                let fid =
                    bytecodegen::bytecode_compile_eval(self, result, outer, Loc::default(), None)?;
                if let Some(class_id) = receiver_class {
                    // The receiver is pushed *onto* the eval site's own
                    // nesting, not in place of it: CRuby resolves a
                    // constant in `Mod.module_eval "Lookup"` through the
                    // receiver first and then the caller's lexical
                    // scope. A block carries no nesting of its own, so
                    // take its mother's.
                    let outer_ctx = {
                        let mut iseq = outer;
                        if self.store[iseq].lexical_context.is_empty() {
                            iseq = self.store[iseq].mother().0;
                        }
                        self.store[iseq].lexical_context.clone()
                    };
                    if let Some(info) = self.store.iseq_mut(fid) {
                        info.lexical_context = outer_ctx;
                        info.lexical_context.push(class_id);
                        // A receiver-anchored eval (`class_eval` /
                        // `instance_eval` with a string) defines
                        // methods on its receiver through the runtime
                        // cref, not at the eval site.
                        info.is_eval = false;
                    }
                }
                #[cfg(feature = "emit-bc")]
                self.dump_bc();
                Ok(fid)
            }
            Err(err) => Err(err),
        }
    }

    /// Returns the byte offset of the source's `DATA` content (just past
    /// its `__END__` line) when present — used by the main-script runner;
    /// other callers ignore it.
    pub fn compile_script_binding(
        &mut self,
        code: Vec<u8>,
        path: impl Into<PathBuf>,
        binding: Binding,
        lineno: i64,
        src_encoding: Option<String>,
    ) -> Result<Option<usize>> {
        self.compile_script_binding_inner(code, path, binding, lineno, src_encoding, false)
    }

    /// `compile_script_binding` for the *main script* run inside
    /// TOPLEVEL_BINDING (`Executor::exec_main_script`). The compiled body
    /// gets a method-style, proc-method Meta — like a `define_method`
    /// body — so a toplevel `return` (bare or from a block) unwinds to
    /// the script frame and terminates the script, instead of walking the
    /// binding's outer chain to a frame that is not on the stack and
    /// raising LocalJumpError.
    pub(crate) fn compile_main_script_binding(
        &mut self,
        code: Vec<u8>,
        path: impl Into<PathBuf>,
        binding: Binding,
    ) -> Result<Option<usize>> {
        self.compile_script_binding_inner(code, path, binding, 1, None, true)
    }

    fn compile_script_binding_inner(
        &mut self,
        code: Vec<u8>,
        path: impl Into<PathBuf>,
        binding: Binding,
        lineno: i64,
        src_encoding: Option<String>,
        main_style: bool,
    ) -> Result<Option<usize>> {
        let line_offset = lineno - 1;
        let outer_fid = binding.outer_fid();
        if self.store[outer_fid].is_iseq().is_none() {
            return Err(MonorubyErr::runtimeerr(
                "eval with binding requires a Ruby method context",
            ));
        }
        // The scope this eval body compiles under. A binding that has
        // already hosted an eval carries its own frame; new code compiles
        // as a CHILD of that frame's iseq, so the locals earlier evals
        // introduced resolve through the outer chain (depth >= 1) into the
        // frame they were born in. They must NOT be re-declared as the new
        // fid's own locals: that came with a value copy into a fresh frame,
        // and any other Binding sharing the old frame (`#dup`) kept seeing
        // the stale copy (#1067).
        let outer = match binding.func_id() {
            Some(fid) => self.store[fid].as_iseq(),
            None => self.store[outer_fid].as_iseq(),
        };
        let external_context = self.store.scoped_locals(outer);

        let (fid, data_offset) = match crate::parser::parse_program_binding(
            code,
            path,
            None,
            Some(&external_context),
            line_offset,
            src_encoding,
            main_style,
        ) {
            Ok(res) => {
                let data_offset = res.data_offset;
                let res =
                    bytecodegen::bytecode_compile_eval(self, res, outer, Loc::default(), None);
                #[cfg(feature = "emit-bc")]
                self.dump_bc();
                res.map(|fid| (fid, data_offset))
            }
            Err(err) => Err(err),
        }?;
        if main_style {
            self.store[fid].set_method_style();
            self.store[fid].set_proc_method();
            // Backtraces must label the main script `<main>`, not
            // `block in <main>` (see `func_description`).
            self.store[fid].set_name(IdentId::_MAIN);
        }
        self.new_binding_frame(fid, binding.self_val(), binding);
        Ok(data_offset)
    }

    pub(crate) fn get_func_data(&mut self, func_id: FuncId) -> &FuncData {
        let info = &self.store[func_id];
        assert!(info.codeptr().is_some());
        info.data_ref()
    }

    ///
    /// Set GC enable flag.
    ///
    /// ### return
    /// GC enable flag before set.
    ///
    pub fn gc_enable(flag: bool) -> bool {
        alloc::set_gc_enabled(flag);
        alloc::ALLOC.with(|alloc| {
            let old = alloc.borrow().gc_enabled;
            alloc.borrow_mut().gc_enabled = flag;
            old
        })
    }

    /// Push monoruby's own stdout buffer out to the kernel.
    ///
    /// `Kernel#p` / `#print` and `$stdout.write` share that one buffer, so
    /// their output can never be reordered relative to each other — which
    /// a second, Rust-side writer over the same fd would allow.
    pub fn flush_stdout(&mut self) -> Result<()> {
        crate::rvalue::io::flush_stdout(&self.store)
    }

    pub fn write_stdout(&mut self, bytes: &[u8]) -> Result<()> {
        crate::rvalue::io::write_stdout(bytes, &self.store)
    }

    pub fn print_value(&mut self, val: Value) -> Result<()> {
        if let Some(s) = val.is_rstring() {
            crate::rvalue::io::write_stdout(&s, &self.store)
        } else {
            let v = val.to_s(&self.store).into_bytes();
            crate::rvalue::io::write_stdout(&v, &self.store)
        }
    }

    // Handling global variables.
    //
    // The plain `set_gvar` / `get_gvar` methods skip hooks and directly touch
    // `Simple` entries. They are used for call sites that assign to pre-known
    // plain globals like `$0`, `$*`, `$!`, where hooking is not involved.
    //
    // The hook-aware entry point is [`GvarTable::get`] / [`GvarTable::set`],
    // which takes `&mut Executor` and is used by the bytecode/JIT runtime
    // trampolines for `LoadGvar` / `StoreGvar`.

    pub fn set_gvar(&mut self, name: IdentId, val: Value) {
        self.gvars.set_simple(name, val);
    }

    pub fn get_gvar(&mut self, name: IdentId) -> Option<Value> {
        self.gvars.get_simple(name)
    }

    /// Register a getter / setter pair for a global variable name.
    ///
    /// `setter == None` makes the variable read-only.
    pub fn define_hooked_variable(
        &mut self,
        name: IdentId,
        getter: GvarGetter,
        setter: Option<GvarSetter>,
    ) {
        self.gvars.define_hook(name, getter, setter);
    }

    /// Alias `new_name` to `old_name`. Subsequent reads and writes of
    /// `new_name` are forwarded to `old_name`'s entry.
    pub fn alias_global_variable(&mut self, new_name: IdentId, old_name: IdentId) {
        self.gvars.define_alias(new_name, old_name);
    }

    ///
    /// Create a new heap frame for *fid* and chain it onto *binding*.
    ///
    /// The frame's outer is the binding's current frame — or the captured
    /// caller frame for a binding that has not hosted an eval yet. Locals
    /// are NOT copied: earlier evals' locals stay in the frame they were
    /// born in and are reached through the outer chain, so every Binding
    /// sharing those frames (`#dup` / `#clone` copies the pointer) reads
    /// and writes the same storage (#1067). Only the locals this fid
    /// itself declares live in the new frame.
    fn new_binding_frame(&mut self, fid: FuncId, self_val: Value, mut binding: Binding) {
        let meta = self.store[fid].meta();
        let mut lfp = Lfp::heap_frame(self_val, meta);
        let outer = match binding.binding() {
            Some(prev) => prev,
            None => binding.outer_lfp(),
        };
        lfp.set_outer(Some(outer));
        binding.set_inner(lfp);
    }

    ///
    /// Create new heap frame with *fid* and *self_val*.
    ///
    /// local variables are copied from *binding_lfp* if any.
    ///
    pub fn new_heap_frame(
        &mut self,
        fid: FuncId,
        self_val: Value,
        binding_lfp: Option<Lfp>,
    ) -> Lfp {
        let meta = self.store[fid].meta();
        let mut lfp = Lfp::heap_frame(self_val, meta);
        if let Some(binding_lfp) = binding_lfp {
            lfp.set_outer(binding_lfp.outer());
            let locals_len = self.locals_len(binding_lfp.func_id());
            for i in SlotId(1)..SlotId(1) + locals_len {
                let v = binding_lfp.register(i);
                // SAFETY: Setting register values during frame initialization.
                // The slot index is within bounds (1..locals_len).
                unsafe { lfp.set_register(i, v) }
            }
        }
        lfp
    }

    // Handling library load path.

    pub fn get_load_path(&self) -> Value {
        self.load_path
    }

    /// Cap the number of backtrace frames printed for uncaught errors
    /// and `Exception#full_message` (`--backtrace-limit=N`).
    pub fn set_backtrace_limit(limit: i64) {
        BACKTRACE_LIMIT.store(limit, std::sync::atomic::Ordering::Relaxed);
    }

    /// The `RUBY_DESCRIPTION` string (e.g. `monoruby 0.3.0 [x86_64-linux]`),
    /// printed by the `-v` / `--version` command-line switches.
    pub fn ruby_description(&self) -> String {
        self.top_string_constant("RUBY_DESCRIPTION")
            .unwrap_or_else(|| format!("{} {}", env!("CARGO_PKG_NAME"), env!("CARGO_PKG_VERSION")))
    }

    /// The `RUBY_COPYRIGHT` string, printed by `--copyright`.
    pub fn ruby_copyright(&self) -> String {
        self.top_string_constant("RUBY_COPYRIGHT")
            .unwrap_or_else(|| format!("{} - Copyright (C) monochrome", env!("CARGO_PKG_NAME")))
    }

    fn top_string_constant(&self, name: &str) -> Option<String> {
        self.store
            .get_constant_noautoload(OBJECT_CLASS, IdentId::get_id(name))
            .and_then(|v| {
                v.is_rstring_inner()
                    .map(|s| String::from_utf8_lossy(s.as_bytes()).into_owned())
            })
    }

    pub fn extend_load_path(&mut self, iter: impl Iterator<Item = String>) {
        self.load_path.as_array().extend(iter.map(Value::string));
    }

    /// Insert directories at the *front* of `$LOAD_PATH` (before the
    /// vendored-stdlib defaults), preserving the iterator's order.
    /// Used for `-I`, `RUBYOPT -I`, and `RUBYLIB`, which CRuby all
    /// places ahead of the built-in paths.
    pub fn prepend_load_path(&mut self, iter: impl Iterator<Item = String>) {
        let dirs: Vec<Value> = iter.map(Value::string).collect();
        self.load_path.as_array().insert_many(0, dirs);
    }

    pub(crate) fn get_loaded_features(&self) -> Value {
        self.loaded_features
    }

    /// Linear scan of `$LOADED_FEATURES` / `$"` for `path`. Used by
    /// `require` / autoload to skip files already loaded. Both the
    /// array entries and `path` are compared via their `OsStr` bytes
    /// so that Ruby-side `$".replace(...)` semantics are honoured.
    pub(crate) fn is_feature_loaded(&self, path: &std::path::Path) -> bool {
        let target = path.as_os_str().as_bytes();
        for v in self.loaded_features.as_array().iter() {
            if let Some(s) = v.is_str()
                && s.as_bytes() == target
            {
                return true;
            }
        }
        false
    }

    /// Append `path` to `$LOADED_FEATURES` if it isn't already
    /// present. Returns `true` if the path was newly added.
    pub(crate) fn add_loaded_feature(&mut self, path: &std::path::Path) -> bool {
        if self.is_feature_loaded(path) {
            return false;
        }
        let value = Value::string_from_str(path.to_string_lossy().as_ref());
        let mut array = self.loaded_features.as_array();
        array.push(value);
        true
    }

    /// Remove the first occurrence of `path` from `$LOADED_FEATURES`,
    /// returning whether anything was removed. Used after a failed
    /// `require` body so the same file can be retried.
    pub(crate) fn remove_loaded_feature(&mut self, path: &std::path::Path) -> bool {
        let target = path.as_os_str().as_bytes();
        let mut array = self.loaded_features.as_array();
        let pos = array
            .iter()
            .position(|v| v.is_str().is_some_and(|s| s.as_bytes() == target));
        match pos {
            Some(idx) => {
                array.remove(idx);
                true
            }
            None => false,
        }
    }

    pub(crate) fn current_source_path(&self, executor: &Executor) -> &std::path::Path {
        let source_func_id = executor.cfp().get_source_pos();
        let sourceinfo = &self.store.iseq(source_func_id).sourceinfo;
        // Prefer the load-time canonical path: `require_relative` (and
        // `__dir__`) must keep resolving against the file's real
        // directory even after a `Dir.chdir` invalidates a relative
        // main-script path.
        match &sourceinfo.absolute_path {
            Some(p) => p,
            None => &sourceinfo.path,
        }
    }

    /// ## ABI of JIT-compiled code.
    ///
    /// ### argument registers:
    ///  - rdi: number pf args
    ///
    /// ### global registers:
    ///  - rbx: &mut Interp
    ///  - r12: &mut Globals
    ///  - r13: pc (dummy for JIT-ed code)
    ///
    /// ## stack layout when just after the code is called
    /// ~~~text
    ///       +-------------+
    /// -0x00 | return addr | <- rsp
    ///       +-------------+
    /// -0x08 |  (old rbp)  |
    ///       +-------------+
    /// -0x10 |    meta     |
    ///       +-------------+
    /// -0x18 |     %0      |
    ///       +-------------+
    /// -0x20 | %1(1st arg) |
    ///       +-------------+
    ///       |             |
    /// ~~~~
    ///
    ///  - meta and arguments is set by caller.
    ///  - (old rbp) is to be set by callee.
    ///
    pub(crate) fn gen_wrapper(&mut self, func_id: FuncId) {
        CODEGEN.with(|codegen| {
            let mut codegen = codegen.borrow_mut();
            #[cfg(feature = "perf")]
            let pair = codegen.get_address_pair();
            let entry = codegen.gen_wrapper(self, func_id);
            let codeptr = codegen.jit.get_label_address(&entry);
            self.store[func_id].set_entry(entry, codeptr);
            #[cfg(feature = "perf")]
            {
                let info = codegen.get_wrapper_info(pair);
                self.store[func_id].set_wrapper_info(info);
            }
        });
    }

    #[cfg_attr(feature = "deopt", track_caller)]
    pub(crate) fn class_version_inc() {
        // Temporary P0 instrumentation: name every bump site. The steady
        // phase of the activerecord workload keeps failing class-version
        // guards (74.9k deopts at Class#new alone, 1.13M salvage passes),
        // so something bumps the version each iteration; the definition
        // hooks (method_added & co.) all report zero, so the bumper is on
        // a path they cannot see.
        #[cfg(feature = "deopt")]
        eprintln!("### class-ver-inc by {}", std::panic::Location::caller());
        CODEGEN.with(|codegen| codegen.borrow_mut().class_version_inc());
    }

    pub(crate) fn class_version() -> u32 {
        CODEGEN.with(|codegen| codegen.borrow().class_version())
    }

    /// Bump the global *constant* version counter, invalidating every
    /// JIT/VM constant lookup cache. Call this whenever the *resolution*
    /// of an existing constant might change without the constant itself
    /// being assigned — for example after `Module#include` /
    /// `Module#prepend` adds a new iclass to a class chain that earlier
    /// callers have already resolved against.
    ///
    /// This entry point has no constant *name* to attribute the change to,
    /// so it also bumps the per-name table's wildcard epoch — every unit's
    /// const salvage fast path is invalidated (see [`const_epoch`]).
    pub(crate) fn const_version_inc() {
        const_epoch::bump_wildcard();
        CODEGEN.with(|codegen| codegen.borrow_mut().const_version_inc());
    }

    pub(crate) fn const_version() -> u64 {
        CODEGEN.with(|codegen| codegen.borrow().const_version())
    }

    pub fn set_constant(&mut self, class_id: ClassId, name: IdentId, val: Value) {
        const_epoch::bump_name(name);
        CODEGEN.with(|codegen| codegen.borrow_mut().const_version_inc());
        self.store.set_constant(class_id, name, val);
    }

    pub fn remove_constant(&mut self, class_id: ClassId, name: IdentId) -> Option<Value> {
        const_epoch::bump_name(name);
        CODEGEN.with(|codegen| codegen.borrow_mut().const_version_inc());
        self[class_id].remove_constant(name)
    }
}

/// Per-name epochs for constant-cache salvage.
///
/// The global const version (a single counter read by every compiled
/// `GuardConstVersion`) moves on *any* constant event anywhere, so a guard
/// failure says nothing about whether the constants a unit actually folded
/// changed. This table refines that: every event that bumps the global
/// version also bumps the epoch of the constant *name* it touched (or the
/// wildcard, when the event has no single name — `include`/`prepend`).
/// A salvage attempt can then prove "none of the names this unit folded
/// were touched" without re-resolving anything.
///
/// Thread-local like `CODEGEN`: each interpreter thread (one per test) has
/// its own table, mirroring the global version counter it refines.
pub(crate) mod const_epoch {
    use crate::IdentId;
    use std::cell::{Cell, RefCell};
    use std::collections::HashMap;

    thread_local! {
        static WILDCARD: Cell<u64> = const { Cell::new(0) };
        static NAMES: RefCell<HashMap<IdentId, u64>> = RefCell::new(HashMap::new());
    }

    pub(crate) fn bump_name(name: IdentId) {
        NAMES.with_borrow_mut(|m| *m.entry(name).or_insert(0) += 1);
    }

    pub(crate) fn bump_wildcard() {
        WILDCARD.with(|w| w.set(w.get() + 1));
    }

    pub(crate) fn name_epoch(name: IdentId) -> u64 {
        NAMES.with_borrow(|m| m.get(&name).copied().unwrap_or(0))
    }

    pub(crate) fn wildcard() -> u64 {
        WILDCARD.with(|w| w.get())
    }
}

impl Globals {
    #[cfg(feature = "profile")]
    pub fn clear_stats(&mut self) {
        self.deopt_stats.clear();
        self.jit_class_unmatched_stats.clear();
        self.jit_recompile_count.clear();
        self.store.clear_stats();
    }

    #[cfg(feature = "profile")]
    pub fn countup_recompile(
        &mut self,
        func_id: FuncId,
        class_id: ClassId,
        reason: &RecompileReason,
    ) {
        match self
            .jit_recompile_count
            .get_mut(&(func_id, class_id, *reason))
        {
            Some(c) => *c += 1,
            None => {
                self.jit_recompile_count
                    .insert((func_id, class_id, *reason), 1);
            }
        };
    }
}

// Program arguments (`ARGV` / `$*` / the ARGF file queue)
impl Globals {
    /// The program-argument array. Both the `ARGV` constant and `$*`
    /// name this same object, and `ARGF` / `Kernel#gets` shift the file
    /// names to read off its front.
    pub fn argv(&self) -> Value {
        self.argv
    }

    /// Install the program-argument array, re-pointing the `ARGV`
    /// constant at it, `$*` (which reads through a hook), and the
    /// process-wide ARGF object's file queue.
    pub fn set_argv(&mut self, argv: Value) {
        self.argv = argv;
        self.set_constant_by_str(OBJECT_CLASS, "ARGV", argv);
        if let Some(mut argf) = self.argf
            && let Some(inner) = argf.try_argf_inner_mut()
        {
            inner.argv = argv;
        }
    }

    /// The `-i[extension]` switch: put the process-wide ARGF into
    /// in-place-edit mode (`""` = no backup files).
    pub fn set_argf_inplace(&mut self, ext: String) {
        if let Some(mut argf) = self.argf
            && let Some(inner) = argf.try_argf_inner_mut()
        {
            inner.inplace = Some(ext);
        }
    }
}

// Random generator
impl Globals {
    /// The exact Integer `srand` reports as the previous seed: the last
    /// explicitly given seed Value, or the system-initialized one.
    pub(crate) fn random_seed_value(&self) -> Value {
        self.random_seed_obj
    }

    /// Re-seed the global PRNG with an explicit Integer seed. Every word
    /// of the seed feeds the Mersenne Twister (CRuby `rand_init`);
    /// `exact` is also what the next `srand` reports back.
    pub(crate) fn random_init_with(&mut self, exact: Value) {
        self.random.seed_value(exact);
        self.random_seed_obj = exact;
    }

    pub(crate) fn random_init(&mut self, seed: Option<i64>) {
        match seed {
            Some(s) => self.random_init_with(Value::integer(s)),
            None => {
                let used = self.random.seed_entropy();
                self.random_seed_obj = Value::integer(used);
            }
        }
    }

    /// Uniform Float in `[0, 1)` (CRuby `genrand_real`, 53-bit).
    pub(crate) fn random_float(&mut self) -> f64 {
        self.random.next_real()
    }

    /// Uniform Float in `[0, 1]`, both ends included (CRuby
    /// `int_pair_to_real_inclusive` — inclusive float-range `rand`).
    pub(crate) fn random_float_inclusive(&mut self) -> f64 {
        self.random.next_real_inclusive()
    }

    /// Uniform integer in `[0, max]` (CRuby `rb_random_ulong_limited` —
    /// the draw `Array#shuffle` / `#sample` and integer `rand` use).
    pub(crate) fn random_ulong_limited(&mut self, max: u64) -> u64 {
        self.random.ulong_limited(max)
    }

    /// `rand(max)` for a positive integer `max`: uniform in `[0, max)`.
    pub(crate) fn random_rand_int(&mut self, max: &num::BigInt) -> Value {
        self.random.rand_int(max)
    }

    pub(crate) fn random_fill_bytes(&mut self, dest: &mut [u8]) {
        self.random.fill_bytes(dest)
    }
}

#[cfg(test)]
mod tests {
    use crate::tests::*;

    #[test]
    fn ruby_constants() {
        // ruby/spec asserts every String-typed RUBY_* constant
        // returns true from `.frozen?`.
        // Ruby 4.0+ exposes a `Ruby` module that mirrors every
        // RUBY_* constant under a non-prefixed name. The mirrors
        // must share *object identity* with the top-level pair, so
        // `equal?` (not just `==`) succeeds.
        run_tests(&[
            r#"RUBY_VERSION.is_a?(String)"#,
            r#"RUBY_DESCRIPTION.is_a?(String)"#,
            r#"RUBY_ENGINE.is_a?(String)"#,
            r#"RUBY_ENGINE_VERSION.is_a?(String)"#,
            r#"RUBY_PLATFORM.is_a?(String)"#,
            r#"RUBY_COPYRIGHT.is_a?(String)"#,
            r#"RUBY_RELEASE_DATE.is_a?(String)"#,
            r#"RUBY_REVISION.is_a?(String)"#,
            r#"RUBY_PATCHLEVEL.is_a?(Integer)"#,
            r#"RUBY_VERSION.frozen?"#,
            r#"RUBY_DESCRIPTION.frozen?"#,
            r#"RUBY_ENGINE.frozen?"#,
            r#"RUBY_ENGINE_VERSION.frozen?"#,
            r#"RUBY_PLATFORM.frozen?"#,
            r#"RUBY_COPYRIGHT.frozen?"#,
            r#"RUBY_RELEASE_DATE.frozen?"#,
            r#"RUBY_REVISION.frozen?"#,
            r#"Ruby.is_a?(Module)"#,
            r#"Ruby::VERSION.equal?(RUBY_VERSION)"#,
            r#"Ruby::DESCRIPTION.equal?(RUBY_DESCRIPTION)"#,
            r#"Ruby::ENGINE.equal?(RUBY_ENGINE)"#,
            r#"Ruby::ENGINE_VERSION.equal?(RUBY_ENGINE_VERSION)"#,
            r#"Ruby::PLATFORM.equal?(RUBY_PLATFORM)"#,
            r#"Ruby::COPYRIGHT.equal?(RUBY_COPYRIGHT)"#,
            r#"Ruby::RELEASE_DATE.equal?(RUBY_RELEASE_DATE)"#,
            r#"Ruby::REVISION.equal?(RUBY_REVISION)"#,
            r#"Ruby::PATCHLEVEL.equal?(RUBY_PATCHLEVEL)"#,
        ]);
    }
}
