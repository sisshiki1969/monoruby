//! Global variable table.
//!
//! All global variables (including special variables like `$~`, `$&`, `$1`,
//! `$LOAD_PATH`, `$stdin`, etc.) live in a single table keyed by [`IdentId`].
//!
//! Each entry in the table is one of:
//!
//! - [`GvarEntry::Simple`] — a plain storage cell holding a [`Value`].
//! - [`GvarEntry::Alias`] — a redirection to another entry, used to implement
//!   `alias $new $old` on global variables.
//! - [`GvarEntry::Hooked`] — an entry whose reads and writes are delegated to
//!   a Rust getter / setter pair. This is analogous to CRuby's
//!   `rb_define_virtual_variable` / `rb_define_hooked_variable` /
//!   `rb_define_readonly_variable`.
//!
//! The table uses a two-level structure: a `HashMap<IdentId, GvarId>` index
//! plus a `Vec<GvarEntry>` store so that entries can be referenced by a stable
//! numeric id (useful for alias redirection and for potential inline caching
//! in the JIT).

use super::*;

/// Opaque identifier for an entry in the [`GvarTable`].
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct GvarId(u32);

impl GvarId {
    fn from_index(idx: usize) -> Self {
        Self(idx as u32)
    }

    fn index(self) -> usize {
        self.0 as usize
    }
}

/// Getter function for a hooked global variable.
///
/// `name` is the [`IdentId`] that was looked up, which lets a single getter
/// service several related names (e.g. a single function reading `$1`..`$9`).
///
/// Returning `None` signals that the variable does not currently exist:
/// reads of the variable evaluate to `nil`, and `defined? $foo` evaluates to
/// `nil`. Returning `Some(value)` signals that the variable exists, even if
/// `value` itself happens to be `nil` — this is the case for `$~` when no
/// match has occurred, which CRuby still reports as defined.
pub type GvarGetter =
    fn(vm: &mut Executor, globals: &mut Globals, name: IdentId) -> Option<Value>;

/// Setter function for a hooked global variable. Returning `Err` raises.
pub type GvarSetter =
    fn(vm: &mut Executor, globals: &mut Globals, name: IdentId, val: Value) -> Result<()>;

/// A single entry in the global variable table.
pub enum GvarEntry {
    /// Plain global variable — value is stored in place.
    Simple(Value),
    /// Alias to another entry.
    Alias(GvarId),
    /// Virtual / hooked global variable serviced by Rust callbacks.
    Hooked {
        getter: GvarGetter,
        setter: Option<GvarSetter>,
    },
}

/// Table of all global variables.
#[derive(Default)]
pub struct GvarTable {
    /// Name → entry id.
    index: HashMap<IdentId, GvarId>,
    /// Entries. Position is stable: entries are never reordered or removed.
    entries: Vec<GvarEntry>,
}

/// Maximum number of alias hops that [`GvarTable::resolve`] will follow before
/// giving up.
const ALIAS_MAX_DEPTH: usize = 16;

impl GvarTable {
    pub fn new() -> Self {
        Self::default()
    }

    /// Look up the (non-alias) entry id for `name`, allocating a fresh
    /// [`GvarEntry::Simple(nil)`] entry on first reference.
    fn entry_id(&mut self, name: IdentId) -> GvarId {
        if let Some(&id) = self.index.get(&name) {
            return id;
        }
        let id = GvarId::from_index(self.entries.len());
        self.entries.push(GvarEntry::Simple(Value::nil()));
        self.index.insert(name, id);
        id
    }

    /// Resolve `id` through any alias chain and return the terminal entry id.
    ///
    /// If the chain is longer than [`ALIAS_MAX_DEPTH`] the original id is
    /// returned — this is a defensive measure against alias cycles introduced
    /// e.g. by `alias $a $b; alias $b $a`.
    fn resolve(&self, mut id: GvarId) -> GvarId {
        for _ in 0..ALIAS_MAX_DEPTH {
            match &self.entries[id.index()] {
                GvarEntry::Alias(next) => id = *next,
                _ => return id,
            }
        }
        id
    }

    /// Read `name`. Returns `nil` when the name is undefined.
    ///
    /// This takes `&mut Executor` and `&mut Globals` because hooked variables
    /// may need to mutate runtime state (e.g. `$~` writes).
    pub fn get(vm: &mut Executor, globals: &mut Globals, name: IdentId) -> Value {
        match Self::lookup(vm, globals, name) {
            Some(v) => v,
            None => {
                // Reading a never-assigned ordinary global warns in
                // verbose mode. Restricted to plain identifier names:
                // punctuation/dash specials read as nil silently, and
                // hidden internals (`$__flip_flop_N`, …) and hooked
                // entries whose getter reported "not present" (`$1`
                // with no match) must not warn either.
                if !globals.gvars.index.contains_key(&name) {
                    let n = name.get_name();
                    if !n.starts_with("$__")
                        && n[1..].starts_with(|c: char| c.is_ascii_alphabetic() || c == '_')
                        && n != "$FILENAME"
                        && verbose_mode(globals)
                    {
                        let _ = vm.ruby_warn(
                            globals,
                            &format!("warning: global variable '{n}' not initialized"),
                        );
                    }
                }
                Value::nil()
            }
        }
    }

    /// Internal lookup that propagates the hook getter's "exists or not"
    /// signal. `Some(v)` means the variable currently exists with value `v`
    /// (which may itself be `nil`, e.g. `$~` after no match); `None` means
    /// the variable does not exist.
    fn lookup(vm: &mut Executor, globals: &mut Globals, name: IdentId) -> Option<Value> {
        let id = globals.gvars.index.get(&name).copied()?;
        let id = globals.gvars.resolve(id);
        match &globals.gvars.entries[id.index()] {
            GvarEntry::Simple(v) => Some(*v),
            GvarEntry::Alias(_) => unreachable!("resolve() follows aliases"),
            GvarEntry::Hooked { getter, .. } => {
                let getter = *getter;
                getter(vm, globals, name)
            }
        }
    }

    /// Runtime `defined?($name)` check.
    ///
    /// Returns `true` when the variable currently exists. For `Simple`
    /// entries that just means "registered". For `Hooked` entries we call
    /// the getter and treat `Some(_)` as defined and `None` as undefined —
    /// hooks for variables CRuby considers always-present (e.g. `$~`,
    /// `$LOAD_PATH`, `$LOADED_FEATURES`) return `Some` even when their
    /// underlying value is nil, while hooks for derived match variables
    /// (`$&`, `$'`, `` $` ``, `$1`..`$N`) return `None` until `$~` is set,
    /// mirroring CRuby's BACK_REF / NTH_REF semantics.
    pub fn defined_runtime(
        vm: &mut Executor,
        globals: &mut Globals,
        name: IdentId,
    ) -> bool {
        Self::lookup(vm, globals, name).is_some()
    }

    /// Write `val` into `name`.
    ///
    /// Returns `Err` if the target is a read-only hooked variable or the
    /// setter itself raises.
    pub fn set(
        vm: &mut Executor,
        globals: &mut Globals,
        name: IdentId,
        val: Value,
    ) -> Result<()> {
        let val = write_special_check(vm, globals, name, val)?;
        // Intern the entry first so we can assign to plain globals without
        // going through the hook path.
        let id = globals.gvars.entry_id(name);
        let id = globals.gvars.resolve(id);
        match &mut globals.gvars.entries[id.index()] {
            GvarEntry::Simple(slot) => {
                *slot = val;
                Ok(())
            }
            GvarEntry::Alias(_) => unreachable!("resolve() follows aliases"),
            GvarEntry::Hooked { setter, .. } => match *setter {
                Some(setter) => setter(vm, globals, name, val),
                None => Err(MonorubyErr::nameerr(format!(
                    "{name} is a read-only variable"
                ))),
            },
        }
    }

    /// Returns `true` if `name` has any entry — Simple, Alias, or Hooked.
    /// Used by the gvar unit tests as a cheap "is the name registered"
    /// probe; runtime `defined?` evaluation goes through
    /// [`GvarTable::defined_runtime`] instead so it can also call hook
    /// getters when appropriate.
    #[cfg(test)]
    pub fn is_defined(&self, name: IdentId) -> bool {
        self.index.contains_key(&name)
    }

    /// Plain getter that only consults [`GvarEntry::Simple`] entries and
    /// ignores hooks. Returns `None` if `name` is unknown or is currently a
    /// hook/alias rather than a simple value.
    ///
    /// This is the fast-path used at pre-known plain global variable names
    /// (e.g. `$0`, `$!`, `$*`) where no hook is ever involved.
    pub fn get_simple(&self, name: IdentId) -> Option<Value> {
        let id = self.index.get(&name).copied()?;
        let id = self.resolve(id);
        if let GvarEntry::Simple(v) = self.entries[id.index()] {
            Some(v)
        } else {
            None
        }
    }

    /// Plain setter that stores into the resolved [`GvarEntry::Simple`]
    /// entry, creating one if necessary. Hooks are bypassed — callers should
    /// use [`GvarTable::set`] instead when hook semantics are required.
    ///
    /// Panics if `name` resolves to a [`GvarEntry::Hooked`] entry: that
    /// combination means Rust-side code is trying to assign to a virtual
    /// variable without going through its setter, which is almost certainly
    /// a bug at the call site.
    pub fn set_simple(&mut self, name: IdentId, val: Value) {
        let id = self.entry_id(name);
        let id = self.resolve(id);
        match &mut self.entries[id.index()] {
            GvarEntry::Simple(slot) => *slot = val,
            GvarEntry::Alias(_) => unreachable!("resolve() follows aliases"),
            GvarEntry::Hooked { .. } => panic!(
                "set_simple called on hooked global variable {name}; \
                 use set_gvar through the hook API instead"
            ),
        }
    }

    /// Install a hooked entry for `name`. Replaces any previously registered
    /// hook/alias, but if `name` already held a `Simple` value its contents
    /// are discarded.
    pub fn define_hook(
        &mut self,
        name: IdentId,
        getter: GvarGetter,
        setter: Option<GvarSetter>,
    ) {
        let id = self.entry_id(name);
        self.entries[id.index()] = GvarEntry::Hooked { getter, setter };
    }

    /// Install an alias: reading or writing `new_name` is redirected to
    /// `old_name`.
    pub fn define_alias(&mut self, new_name: IdentId, old_name: IdentId) {
        let target = self.entry_id(old_name);
        let target = self.resolve(target);
        let id = self.entry_id(new_name);
        if id == target {
            // alias $x $x — nothing to do.
            return;
        }
        self.entries[id.index()] = GvarEntry::Alias(target);
    }

    /// Iterate over the [`Value`]s stored directly in [`GvarEntry::Simple`]
    /// entries so the GC can mark them. Hooked entries are not iterated here;
    /// their backing storage (e.g. [`Globals::load_path`]) is marked through
    /// its owner.
    pub fn mark_values<F: FnMut(Value)>(&self, mut f: F) {
        for entry in &self.entries {
            if let GvarEntry::Simple(v) = entry {
                f(*v);
            }
        }
    }
}

/// `$VERBOSE` is exactly `true` (nil = quiet, false = normal).
fn verbose_mode(globals: &mut Globals) -> bool {
    globals
        .get_gvar(IdentId::get_id("$VERBOSE"))
        .is_some_and(|v| v.as_bool())
}

/// Deprecation warning for the record/field separator globals (nil
/// assignments are fine).
fn warn_deprecated_separator(vm: &mut Executor, globals: &mut Globals, name: &str, val: Value) {
    if !val.is_nil() {
        warn_deprecated(vm, globals, &format!("warning: non-nil '{name}' is deprecated"));
    }
}

/// Emit a deprecation warning through `Kernel#__warn_deprecated` so
/// CRuby's gating applies: silent unless `Warning[:deprecated]` is
/// enabled or `$VERBOSE` is true, and a user-overridden Warning.warn
/// takes effect. The helper lives in startup.rb, so warnings during
/// bootstrap (before it exists) stay silent, and a failing warning
/// never turns the triggering operation into an error.
fn warn_deprecated(vm: &mut Executor, globals: &mut Globals, msg: &str) {
    let main = globals.main_object;
    if globals
        .store
        .check_method(main, IdentId::get_id("__warn_deprecated"))
        .is_none()
    {
        return;
    }
    let msg = Value::string(msg.to_string());
    let _ = vm.invoke_method_inner(
        globals,
        IdentId::get_id("__warn_deprecated"),
        main,
        &[msg],
        None,
        None,
    );
}

/// CRuby installs setter hooks on a number of special globals that
/// validate or coerce the assigned value, or reject the write outright.
/// monoruby keeps most of them as plain [`GvarEntry::Simple`] storage
/// because Rust-side writers update them through `set_simple` (which
/// must stay hook-free), so the Ruby-level assignment path applies the
/// equivalent checks by name here instead. Returns the (possibly
/// coerced) value to store.
fn write_special_check(
    vm: &mut Executor,
    globals: &mut Globals,
    name: IdentId,
    val: Value,
) -> Result<Value> {
    /// `$/`-style setter (Ruby 4.0 semantics): the value must be a
    /// String or nil. Unless it is already a frozen bare String, it is
    /// replaced by a frozen plain-String copy (subclass and instance
    /// variables are dropped, like CRuby's `rb_str_new_frozen`).
    fn frozen_string_or_nil(name: &str, val: Value) -> Result<Value> {
        if val.is_nil() {
            return Ok(val);
        }
        let Some(s) = val.is_rstring_inner() else {
            return Err(MonorubyErr::typeerr(format!(
                "value of {name} must be String"
            )));
        };
        if val.is_frozen() && val.class() == STRING_CLASS {
            return Ok(val);
        }
        let mut v = Value::string_from_inner(s.clone());
        v.set_frozen();
        Ok(v)
    }

    let n = name.get_name();
    match n.as_str() {
        // Read-only variables backed by plain storage: `$?` is updated
        // from Rust via `set_simple` (which bypasses this check), the
        // others have no writer at all. `$!` is read-only in CRuby
        // too, but monoruby's rescue bookkeeping bytecode stores to it
        // through this path, so it cannot be listed.
        "$?" | "$<" | "$FILENAME" => Err(MonorubyErr::nameerr(format!(
            "{n} is a read-only variable"
        ))),
        // The input record separator: String or nil, stored as a
        // frozen plain-String copy. `$-0` is an alias of `$/`
        // (registered in `init_builtin_gvars`), but the alias name
        // still reaches this check, so match it too for the error
        // message's sake.
        "$/" | "$-0" => {
            let val = frozen_string_or_nil(&n, val)?;
            warn_deprecated_separator(vm, globals, &n, val);
            Ok(val)
        }
        // Output record / field separators: String or nil, stored
        // as-is (CRuby does not copy these).
        "$\\" | "$," => {
            if val.is_nil() || val.is_rstring().is_some() {
                warn_deprecated_separator(vm, globals, &n, val);
                Ok(val)
            } else {
                Err(MonorubyErr::typeerr(format!(
                    "value of {n} must be String"
                )))
            }
        }
        // The input field separator additionally accepts a Regexp.
        "$;" => {
            if val.is_nil() || val.is_rstring().is_some() || val.is_regex().is_some() {
                warn_deprecated_separator(vm, globals, &n, val);
                Ok(val)
            } else {
                Err(MonorubyErr::typeerr(format!(
                    "value of {n} must be String or Regexp"
                )))
            }
        }
        // `$.` (last input line number) coerces via `#to_int`.
        "$." => Ok(Value::integer(val.coerce_to_int_i64(vm, globals)?)),
        // `$stdout` / `$stderr` accept any object with a `write`
        // method.
        "$stdout" | "$stderr" => {
            if globals
                .store
                .check_method_for_class(val.class(), IdentId::get_id("write"))
                .is_some()
            {
                Ok(val)
            } else {
                Err(MonorubyErr::typeerr(format!(
                    "{n} must have write method, {} given",
                    val.get_real_class_name(&globals.store)
                )))
            }
        }
        // `$VERBOSE` normalizes any truthy assignment to `true`
        // (nil and false are stored as-is).
        "$VERBOSE" | "$-v" | "$-w" => Ok(if val.as_bool() {
            Value::bool(true)
        } else {
            val
        }),
        // `$0` requires a String — and actually renames the process
        // (CRuby's setproctitle), so `ps` shows the new title.
        "$0" | "$PROGRAM_NAME" => {
            if let Some(inner) = val.is_rstring_inner() {
                set_process_title(inner.as_bytes());
                Ok(val)
            } else {
                Err(MonorubyErr::no_implicit_conversion(
                    &globals.store,
                    val,
                    STRING_CLASS,
                ))
            }
        }
        _ => Ok(val),
    }
}

/// Overwrite the process's argv area so `ps -ocommand` shows *title* —
/// what CRuby's `$0 =` does via setproctitle. Linux-only: the argv
/// range comes from `/proc/self/stat`'s `arg_start`/`arg_end` fields
/// (48/49, Linux ≥ 3.5); a no-op elsewhere or when they are
/// unavailable. Also sets the comm name (`PR_SET_NAME`) so
/// `ps -ocomm` follows, like CRuby.
fn set_process_title(title: &[u8]) {
    #[cfg(target_os = "linux")]
    {
        let Ok(stat) = std::fs::read_to_string("/proc/self/stat") else {
            return;
        };
        // The comm field (2nd) may contain spaces/parens; fields are
        // reliable only after the LAST ')'.
        let Some(rest) = stat.rfind(')').map(|i| &stat[i + 2..]) else {
            return;
        };
        let fields: Vec<&str> = rest.split(' ').collect();
        // `rest` starts at field 3 (state), so arg_start/arg_end
        // (fields 48/49) sit at indices 45/46.
        let (Some(start), Some(end)) = (
            fields.get(45).and_then(|s| s.parse::<usize>().ok()),
            fields.get(46).and_then(|s| s.parse::<usize>().ok()),
        ) else {
            return;
        };
        if start == 0 || end <= start {
            return;
        }
        let len = end - start;
        let n = title.len().min(len - 1);
        // SAFETY: [arg_start, arg_end) is this process's own argv
        // area, mapped writable; we never write past `end`.
        unsafe {
            let p = start as *mut u8;
            std::ptr::copy_nonoverlapping(title.as_ptr(), p, n);
            std::ptr::write_bytes(p.add(n), 0, len - n);
        }
        // comm is capped at 15 bytes + NUL.
        let mut comm = [0u8; 16];
        let cn = title.len().min(15);
        comm[..cn].copy_from_slice(&title[..cn]);
        // SAFETY: PR_SET_NAME reads a NUL-terminated string of at most
        // 16 bytes from the pointer; `comm` satisfies that.
        unsafe {
            libc::prctl(libc::PR_SET_NAME, comm.as_ptr());
        }
    }
    #[cfg(not(target_os = "linux"))]
    {
        let _ = title;
    }
}

///
/// Register hook-based special variables at `Globals::new` time.
///
/// All `$~`, `$&`, `$'`, `$1`..`$9`, `$LOAD_PATH` / `$:`, `$LOADED_FEATURES`
/// / `$"` are installed here as [`GvarEntry::Hooked`] entries so they can be
/// freely `alias`ed and so no bytecode or runtime code needs to know their
/// names by hand.
///
pub fn init_builtin_gvars(globals: &mut Globals) {
    // --- Exception being handled (`$!`) ---------------------------------------

    // `$!` is per execution context in CRuby — each Fiber sees its own
    // errinfo — so it is backed by `Executor::errinfo` rather than the
    // process-global gvar table, and it is read-only from Ruby. The VM's
    // rescue-entry path writes the field directly; the bytecodegen
    // save/restore protocol (rescue-exit restore, `defined?` probes)
    // writes it through the internal `$(errinfo)` hook, whose name cannot
    // be spelled in Ruby source.
    fn get_errinfo(
        vm: &mut Executor,
        _globals: &mut Globals,
        _name: IdentId,
    ) -> Option<Value> {
        Some(vm.errinfo())
    }

    fn set_errinfo_internal(
        vm: &mut Executor,
        _globals: &mut Globals,
        _name: IdentId,
        val: Value,
    ) -> Result<()> {
        vm.set_errinfo(val);
        Ok(())
    }

    // `$$` — the current process id, read-only.
    fn get_pid(_vm: &mut Executor, _globals: &mut Globals, _name: IdentId) -> Option<Value> {
        Some(Value::integer(std::process::id() as i64))
    }
    globals.define_hooked_variable(IdentId::get_id("$$"), get_pid, None);

    globals.define_hooked_variable(IdentId::get_id("$!"), get_errinfo, None);
    globals.define_hooked_variable(
        IdentId::get_id(crate::globals::ERRINFO_INTERNAL_GVAR),
        get_errinfo,
        Some(set_errinfo_internal),
    );

    // --- Regexp-related special variables -----------------------------------

    // $~ is always considered defined (CRuby parses it as a regular global
    // variable reference); the getter therefore always returns `Some`,
    // wrapping `nil` when no match has occurred yet.
    fn get_match_data(
        vm: &mut Executor,
        _globals: &mut Globals,
        _name: IdentId,
    ) -> Option<Value> {
        Some(vm.get_last_matchdata())
    }

    fn set_match_data(
        vm: &mut Executor,
        globals: &mut Globals,
        _name: IdentId,
        val: Value,
    ) -> Result<()> {
        // CRuby only allows `$~ = nil | MatchData`: nil clears the
        // capture state, a MatchData replaces it (so the derived
        // globals `$&`, `$1`, ... immediately reflect it), anything
        // else is a TypeError.
        if val.is_nil() {
            vm.clear_capture_special_variables();
            Ok(())
        } else if val
            .try_rvalue()
            .is_some_and(|rv| rv.ty() == ObjTy::MATCHDATA)
        {
            vm.set_backref(val);
            Ok(())
        } else {
            Err(MonorubyErr::typeerr(format!(
                "wrong argument type {} (expected MatchData)",
                val.get_real_class_name(&globals.store)
            )))
        }
    }

    // $&, $', $`, $1..$N are derived from $~ and CRuby parses them as
    // BACK_REF / NTH_REF nodes. Each getter returns `None` until $~ has
    // been populated by a successful match, so `defined?` mirrors CRuby
    // and reports nil in the unset state.
    fn get_last_match(
        vm: &mut Executor,
        _globals: &mut Globals,
        _name: IdentId,
    ) -> Option<Value> {
        vm.sp_last_match()
    }

    fn get_post_match(
        vm: &mut Executor,
        _globals: &mut Globals,
        _name: IdentId,
    ) -> Option<Value> {
        vm.sp_post_match()
    }

    fn get_pre_match(
        vm: &mut Executor,
        _globals: &mut Globals,
        _name: IdentId,
    ) -> Option<Value> {
        vm.sp_pre_match()
    }

    fn get_last_paren_match(
        vm: &mut Executor,
        _globals: &mut Globals,
        _name: IdentId,
    ) -> Option<Value> {
        vm.sp_last_paren_match()
    }

    /// `$1`, `$2`, ... — `name` is of the form `$n`. Strip the `$` and parse
    /// the decimal digits; any non-numeric suffix yields `None`.
    fn get_match_nth(
        vm: &mut Executor,
        _globals: &mut Globals,
        name: IdentId,
    ) -> Option<Value> {
        let s = name.get_name();
        let rest = s.strip_prefix('$')?;
        let n = rest.parse::<i64>().ok()?;
        vm.get_special_matches(n)
    }

    globals.define_hooked_variable(
        IdentId::get_id("$~"),
        get_match_data,
        Some(set_match_data),
    );
    globals.define_hooked_variable(IdentId::get_id("$&"), get_last_match, None);
    globals.define_hooked_variable(IdentId::get_id("$'"), get_post_match, None);
    globals.define_hooked_variable(IdentId::get_id("$`"), get_pre_match, None);
    // `$+` — the last non-empty captured group of the current match.
    globals.define_hooked_variable(IdentId::get_id("$+"), get_last_paren_match, None);

    // `$_` (last line read by `gets` / `readline`) is frame-local in
    // CRuby — it lives in `vm_svar.lastline` alongside `$~`. monoruby
    // stores it in slot `SVAR_LASTLINE` of the LEP's container, so it
    // routes through the same hooked-variable machinery rather than the
    // process-global gvar table.
    fn get_last_read_line(
        vm: &mut Executor,
        _globals: &mut Globals,
        _name: IdentId,
    ) -> Option<Value> {
        Some(vm.get_last_read_line())
    }

    fn set_last_read_line(
        vm: &mut Executor,
        _globals: &mut Globals,
        _name: IdentId,
        val: Value,
    ) -> Result<()> {
        vm.set_last_read_line(val);
        Ok(())
    }

    globals.define_hooked_variable(
        IdentId::get_id("$_"),
        get_last_read_line,
        Some(set_last_read_line),
    );

    // $1 through $9 are the common case; higher-numbered matches are also
    // allowed by name — they all share the same getter which parses the
    // numeric suffix out of `name`.
    for n in 1..=9 {
        globals.define_hooked_variable(
            IdentId::get_id(&format!("${}", n)),
            get_match_nth,
            None,
        );
    }

    // --- Library load path --------------------------------------------------

    fn get_load_path_hook(
        _vm: &mut Executor,
        globals: &mut Globals,
        _name: IdentId,
    ) -> Option<Value> {
        Some(globals.get_load_path())
    }

    globals.define_hooked_variable(
        IdentId::get_id("$LOAD_PATH"),
        get_load_path_hook,
        None,
    );
    // `$:` and `$-I` are aliases of `$LOAD_PATH`.
    globals.alias_global_variable(IdentId::get_id("$:"), IdentId::get_id("$LOAD_PATH"));
    globals.alias_global_variable(IdentId::get_id("$-I"), IdentId::get_id("$LOAD_PATH"));

    // --- Loaded features ----------------------------------------------------

    fn get_loaded_features_hook(
        _vm: &mut Executor,
        globals: &mut Globals,
        _name: IdentId,
    ) -> Option<Value> {
        Some(globals.get_loaded_features())
    }

    globals.define_hooked_variable(
        IdentId::get_id("$LOADED_FEATURES"),
        get_loaded_features_hook,
        None,
    );
    globals.alias_global_variable(
        IdentId::get_id("$\""),
        IdentId::get_id("$LOADED_FEATURES"),
    );

    // --- Record separator ---------------------------------------------------

    // `$/` (the input record separator, used as the default argument of
    // `String#chomp`, `IO#gets`, `String#lines`, …) defaults to "\n" in
    // CRuby. It is a plain, assignable global; seed the default value so
    // reads return "\n" before any assignment. `$-0` is its alias.
    let mut sep = Value::string_from_str("\n");
    sep.set_frozen();
    globals.set_gvar(IdentId::get_id("$/"), sep);
    globals.alias_global_variable(IdentId::get_id("$-0"), IdentId::get_id("$/"));

    // --- Exception backtrace --------------------------------------------------

    // `$@` delegates to `$!`: reading returns `$!.backtrace` (nil when
    // no exception is set), writing calls `$!.set_backtrace` (which
    // validates the value) and raises ArgumentError when `$!` is not
    // set. CRuby reports `defined?($@)` as "global-variable"
    // unconditionally, so the getter always returns `Some`.
    fn get_errinfo_backtrace(
        vm: &mut Executor,
        globals: &mut Globals,
        _name: IdentId,
    ) -> Option<Value> {
        let err = vm.errinfo();
        if err.is_nil() {
            return Some(Value::nil());
        }
        match vm.invoke_method_simple(globals, IdentId::get_id("backtrace"), err, &[]) {
            Some(v) => Some(v),
            None => {
                // Don't leave a pending error behind a plain read.
                let _ = vm.take_error();
                Some(Value::nil())
            }
        }
    }

    fn set_errinfo_backtrace(
        vm: &mut Executor,
        globals: &mut Globals,
        _name: IdentId,
        val: Value,
    ) -> Result<()> {
        let err = vm.errinfo();
        if err.is_nil() {
            return Err(MonorubyErr::argumenterr("$! not set"));
        }
        match vm.invoke_method_simple(globals, IdentId::get_id("set_backtrace"), err, &[val]) {
            Some(_) => Ok(()),
            None => Err(vm.take_error()),
        }
    }

    globals.define_hooked_variable(
        IdentId::get_id("$@"),
        get_errinfo_backtrace,
        Some(set_errinfo_backtrace),
    );

    // `$.` (last input line number) defaults to 0 in CRuby. Seeding it
    // matters beyond fidelity: its Ruby-level setter coerces with
    // `#to_int`, and code like rubygems' StubSpecification saves and
    // restores `$.` verbatim — a nil default would blow up the
    // restore.
    globals.set_gvar(IdentId::get_id("$."), Value::integer(0));

    // `$=` (the pre-1.9 case-insensitivity toggle) is a defunct
    // special: it reads as false and assignments are ignored, with a
    // verbose-mode warning on both, matching CRuby.
    fn get_ignorecase(
        vm: &mut Executor,
        globals: &mut Globals,
        _name: IdentId,
    ) -> Option<Value> {
        warn_deprecated(vm, globals, "warning: variable $= is no longer effective");
        Some(Value::bool(false))
    }

    fn set_ignorecase(
        vm: &mut Executor,
        globals: &mut Globals,
        _name: IdentId,
        _val: Value,
    ) -> Result<()> {
        warn_deprecated(
            vm,
            globals,
            "warning: variable $= is no longer effective; ignored",
        );
        Ok(())
    }

    globals.define_hooked_variable(
        IdentId::get_id("$="),
        get_ignorecase,
        Some(set_ignorecase),
    );

    // --- Command-line flag mirrors -------------------------------------------

    // `$-a`, `$-l`, `$-p` mirror the corresponding CLI switches, none of
    // which monoruby implements, so they read as `false` and are
    // read-only like in CRuby.
    fn get_false(
        _vm: &mut Executor,
        _globals: &mut Globals,
        _name: IdentId,
    ) -> Option<Value> {
        Some(Value::bool(false))
    }

    for flag in ["$-a", "$-l", "$-p"] {
        globals.define_hooked_variable(IdentId::get_id(flag), get_false, None);
    }
}

// ============================================================================
// Unit tests
//
// These tests cover the pure-Rust internals of `GvarTable` (allocation, alias
// resolution, hook lookup, cycle detection) without needing a full
// `Globals`/`Executor` to be constructed. End-to-end Ruby-level behaviour for
// `$~`, `$&`, `$1..$9`, alias chains, and read-only hooks is exercised by
// the integration tests in `monoruby/src/builtins/class.rs`.
// ============================================================================
#[cfg(test)]
mod tests {
    use super::*;

    fn n(s: &str) -> IdentId {
        IdentId::get_id(s)
    }

    /// Dummy getter that reports the variable as existing with value nil.
    /// We can't actually fire hooks in these tests because the
    /// [`GvarGetter`] signature requires `&mut Executor` and `&mut Globals`,
    /// which would be expensive to construct. The hook *registration* and
    /// *resolution* are what we test here; the live invocation is covered
    /// by the Ruby-level integration tests.
    fn dummy_get(
        _vm: &mut Executor,
        _g: &mut Globals,
        _n: IdentId,
    ) -> Option<Value> {
        Some(Value::nil())
    }

    fn dummy_set(
        _vm: &mut Executor,
        _g: &mut Globals,
        _n: IdentId,
        _v: Value,
    ) -> Result<()> {
        Ok(())
    }

    #[test]
    fn simple_set_get_round_trip() {
        let mut t = GvarTable::new();
        t.set_simple(n("$test_a"), Value::integer(42));
        assert_eq!(t.get_simple(n("$test_a")), Some(Value::integer(42)));
    }

    #[test]
    fn unknown_name_returns_none_and_is_not_defined() {
        let t = GvarTable::new();
        assert!(t.get_simple(n("$test_undefined_xxxx")).is_none());
        assert!(!t.is_defined(n("$test_undefined_xxxx")));
    }

    #[test]
    fn is_defined_recognizes_simple_alias_and_hook() {
        let mut t = GvarTable::new();
        t.set_simple(n("$test_def_simple"), Value::integer(1));
        t.define_hook(n("$test_def_hook"), dummy_get, None);
        t.define_alias(n("$test_def_alias"), n("$test_def_simple"));
        assert!(t.is_defined(n("$test_def_simple")));
        assert!(t.is_defined(n("$test_def_hook")));
        assert!(t.is_defined(n("$test_def_alias")));
        assert!(!t.is_defined(n("$test_def_unknown")));
    }

    #[test]
    fn alias_redirects_simple_writes_and_reads() {
        // alias $test_b -> $test_a; writing $test_b should land in $test_a's
        // slot and vice versa.
        let mut t = GvarTable::new();
        t.set_simple(n("$test_a"), Value::integer(1));
        t.define_alias(n("$test_b"), n("$test_a"));
        // read through alias
        assert_eq!(t.get_simple(n("$test_b")), Some(Value::integer(1)));
        // write through alias
        t.set_simple(n("$test_b"), Value::integer(99));
        assert_eq!(t.get_simple(n("$test_a")), Some(Value::integer(99)));
        assert_eq!(t.get_simple(n("$test_b")), Some(Value::integer(99)));
    }

    #[test]
    fn alias_chain_resolves_to_terminal_entry() {
        // $c -> $b -> $a, all reads/writes land in $a's slot.
        let mut t = GvarTable::new();
        t.set_simple(n("$test_chain_a"), Value::integer(0));
        t.define_alias(n("$test_chain_b"), n("$test_chain_a"));
        t.define_alias(n("$test_chain_c"), n("$test_chain_b"));
        t.set_simple(n("$test_chain_c"), Value::integer(7));
        assert_eq!(t.get_simple(n("$test_chain_a")), Some(Value::integer(7)));
        assert_eq!(t.get_simple(n("$test_chain_b")), Some(Value::integer(7)));
        assert_eq!(t.get_simple(n("$test_chain_c")), Some(Value::integer(7)));
    }

    #[test]
    fn alias_cycle_does_not_infinite_loop() {
        // Construct $x -> $y -> $x and verify resolve() bails out instead of
        // looping forever.
        let mut t = GvarTable::new();
        t.set_simple(n("$test_cycle_x"), Value::integer(1));
        t.set_simple(n("$test_cycle_y"), Value::integer(2));
        t.define_alias(n("$test_cycle_x"), n("$test_cycle_y"));
        t.define_alias(n("$test_cycle_y"), n("$test_cycle_x"));
        // get_simple just needs to terminate; the value it returns is
        // unspecified once a cycle is involved.
        let _ = t.get_simple(n("$test_cycle_x"));
        let _ = t.get_simple(n("$test_cycle_y"));
        // is_defined should still return true.
        assert!(t.is_defined(n("$test_cycle_x")));
        assert!(t.is_defined(n("$test_cycle_y")));
    }

    #[test]
    fn define_hook_replaces_simple_entry() {
        let mut t = GvarTable::new();
        t.set_simple(n("$test_replace"), Value::integer(123));
        // After installing a hook, get_simple should report None because the
        // entry is now Hooked, not Simple.
        t.define_hook(n("$test_replace"), dummy_get, Some(dummy_set));
        assert!(t.get_simple(n("$test_replace")).is_none());
        assert!(t.is_defined(n("$test_replace")));
    }

    #[test]
    fn define_hook_with_no_setter_is_read_only() {
        // We can't actually fire the hook here without a live Globals, but we
        // can verify the entry was installed and that get_simple correctly
        // reports None for it (so that the unified `defined?` path uses
        // `is_defined` instead of `get_simple`).
        let mut t = GvarTable::new();
        t.define_hook(n("$test_ro"), dummy_get, None);
        assert!(t.get_simple(n("$test_ro")).is_none());
        assert!(t.is_defined(n("$test_ro")));
    }

    #[test]
    fn alias_to_hook_resolves_through_to_hook_entry() {
        // alias $test_h_alias -> $test_h_hook (a hooked entry).
        // get_simple should still report None because the resolved entry is
        // Hooked, but is_defined should report true.
        let mut t = GvarTable::new();
        t.define_hook(n("$test_h_hook"), dummy_get, None);
        t.define_alias(n("$test_h_alias"), n("$test_h_hook"));
        assert!(t.get_simple(n("$test_h_alias")).is_none());
        assert!(t.is_defined(n("$test_h_alias")));
    }

    #[test]
    fn alias_to_self_is_a_noop() {
        // `alias $x $x` must not destroy the entry.
        let mut t = GvarTable::new();
        t.set_simple(n("$test_self_alias"), Value::integer(11));
        t.define_alias(n("$test_self_alias"), n("$test_self_alias"));
        assert_eq!(
            t.get_simple(n("$test_self_alias")),
            Some(Value::integer(11))
        );
    }

    #[test]
    #[should_panic(expected = "set_simple called on hooked global variable")]
    fn set_simple_panics_on_hook() {
        // The hardening introduced in Phase A: Rust-side mistakes that try
        // to plain-write to a hooked variable surface immediately as a
        // panic instead of silently bypassing the hook.
        let mut t = GvarTable::new();
        t.define_hook(n("$test_panic_hook"), dummy_get, Some(dummy_set));
        t.set_simple(n("$test_panic_hook"), Value::integer(0));
    }

    #[test]
    fn mark_values_visits_only_simple_entries() {
        // Hooked entries' backing storage is owned elsewhere (e.g.
        // `Globals::load_path`); only Simple slots should be visited by the
        // GC marker.
        let mut t = GvarTable::new();
        t.set_simple(n("$test_mark_simple"), Value::integer(5));
        t.define_hook(n("$test_mark_hook"), dummy_get, None);
        let mut visited = 0;
        t.mark_values(|_| visited += 1);
        assert_eq!(visited, 1);
    }
}
