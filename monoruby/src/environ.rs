//! The process environment, as the interpreter reads and writes it.
//!
//! `ENV` keeps no copy of the environment: native code (a C extension,
//! a library it calls) changes it with `setenv(3)` / `unsetenv(3)` behind
//! monoruby's back, and CRuby's `ENV` asks libc on every access (#2109).
//! So does everything here, and it does so on the bytes `environ` holds:
//! a name or a value is whatever bytes the kernel handed the process,
//! not necessarily text in any encoding (#2135, #2136).
//!
//! Reading goes to `environ` and `getenv(3)` directly rather than through
//! `std::env`, whose only way to enumerate (`vars_os`) allocates two
//! `OsString`s per variable — for `ENV.size` as much as for `ENV.to_h`.
//!
//! # Locking
//!
//! `setenv(3)` may reallocate the `environ` array, so a walk must not
//! overlap a write. Within one interpreter the GVL already guarantees
//! that: every function here is called by Ruby code holding it. [`LOCK`]
//! is for a process running several interpreters (the test harness),
//! each under its own GVL: readers hold it shared, writers exclusively.
//! Writers still go through `std::env`, whose own lock keeps them apart
//! from the standard library's readers (`std::process::Command`,
//! `std::env::var`). What native code does to the environment meanwhile
//! is as safe as it is on CRuby.

use std::ffi::{CStr, OsStr};
use std::os::unix::ffi::OsStrExt;
use std::sync::{RwLock, RwLockReadGuard, RwLockWriteGuard};

static LOCK: RwLock<()> = RwLock::new(());

fn read_lock() -> RwLockReadGuard<'static, ()> {
    LOCK.read().unwrap_or_else(|poison| poison.into_inner())
}

fn write_lock() -> RwLockWriteGuard<'static, ()> {
    LOCK.write().unwrap_or_else(|poison| poison.into_inner())
}

/// The address of the first entry of `environ`.
///
/// # Safety
///
/// The caller holds [`LOCK`], and reads the array before releasing it.
unsafe fn environ_ptr() -> *const *const libc::c_char {
    #[cfg(target_vendor = "apple")]
    {
        // SAFETY: `_NSGetEnviron` always answers the address of the
        // process's `environ`.
        unsafe { *libc::_NSGetEnviron() as *const *const libc::c_char }
    }
    #[cfg(not(target_vendor = "apple"))]
    {
        unsafe extern "C" {
            static environ: *const *const libc::c_char;
        }
        // SAFETY: a plain read of the pointer; the caller holds the lock.
        unsafe { environ }
    }
}

/// Walk the environment in place, without copying anything: `f` is given
/// each variable's name and value and answers whether to go on.
///
/// An entry is split at its first `=`, and one with none is skipped, as
/// CRuby's `ENV` does. `f` runs with the lock held, so it must neither
/// touch the environment nor run Ruby code.
pub(crate) fn walk(mut f: impl FnMut(&[u8], &[u8]) -> bool) {
    let _guard = read_lock();
    // SAFETY: `environ` is a NULL-terminated array of NUL-terminated
    // strings, and no writer of ours runs while the lock is held.
    unsafe {
        let mut entry = environ_ptr();
        if entry.is_null() {
            return;
        }
        while !(*entry).is_null() {
            let bytes = CStr::from_ptr(*entry).to_bytes();
            if let Some(eq) = memchr::memchr(b'=', bytes)
                && !f(&bytes[..eq], &bytes[eq + 1..])
            {
                return;
            }
            entry = entry.add(1);
        }
    }
}

/// The environment itself, for an `execve` that hands it on unchanged:
/// the lock is held for as long as this lives, so the array stays where
/// it is until the child has it. Nothing that touches the environment
/// may run meanwhile.
pub(crate) struct Inherited {
    _guard: RwLockReadGuard<'static, ()>,
}

pub(crate) fn inherit() -> Inherited {
    let _guard = read_lock();
    Inherited { _guard }
}

impl Inherited {
    /// `environ`, as `execve`'s `envp`.
    pub(crate) fn as_ptr(&self) -> *const *const libc::c_char {
        // SAFETY: the lock is held.
        or_empty(unsafe { environ_ptr() })
    }
}

/// `envp`, or an empty array for the `NULL` that `clearenv(3)` leaves in
/// `environ`: only Linux's `execve` takes that for "no variables".
fn or_empty(envp: *const *const libc::c_char) -> *const *const libc::c_char {
    static EMPTY: [usize; 1] = [0];
    if envp.is_null() {
        EMPTY.as_ptr().cast()
    } else {
        envp
    }
}

/// The number of entries in the environment (`ENV.size`).
///
/// Every entry counts, with or without a `=`, as in CRuby — which makes
/// this a walk over the pointers alone.
pub(crate) fn count() -> usize {
    let _guard = read_lock();
    // SAFETY: as in `walk`.
    unsafe {
        let first = environ_ptr();
        if first.is_null() {
            return 0;
        }
        let mut entry = first;
        while !(*entry).is_null() {
            entry = entry.add(1);
        }
        entry.offset_from_unsigned(first)
    }
}

/// Whether the environment has no entry at all (`ENV.empty?`).
pub(crate) fn is_empty() -> bool {
    let _guard = read_lock();
    // SAFETY: as in `walk`.
    unsafe {
        let first = environ_ptr();
        first.is_null() || (*first).is_null()
    }
}

/// `getenv(name)`: `f` is given the value's bytes where they are, under
/// the lock, when the variable is set.
///
/// A name with a NUL in it names no variable.
pub(crate) fn get<R>(name: &[u8], f: impl FnOnce(&[u8]) -> R) -> Option<R> {
    // The name has to be NUL-terminated for libc; the usual one fits a
    // buffer on the stack.
    let mut stack = [0u8; 128];
    let heap;
    let cname: &[u8] = if name.len() < stack.len() {
        stack[..name.len()].copy_from_slice(name);
        &stack[..=name.len()]
    } else {
        heap = [name, &[0]].concat();
        &heap
    };
    let cname = CStr::from_bytes_with_nul(cname).ok()?;
    let _guard = read_lock();
    // SAFETY: `cname` is NUL-terminated, and the value `getenv` points
    // into stays where it is while the lock is held.
    unsafe {
        let value = libc::getenv(cname.as_ptr());
        if value.is_null() {
            None
        } else {
            Some(f(CStr::from_ptr(value).to_bytes()))
        }
    }
}

/// Whether the variable `name` is set.
pub(crate) fn is_set(name: &[u8]) -> bool {
    get(name, |_| ()).is_some()
}

/// The name of the first variable whose value is byte for byte `value`
/// (`ENV.key`, `ENV.rassoc`, `ENV.value?`).
pub(crate) fn name_of_value(value: &[u8]) -> Option<Vec<u8>> {
    let mut found = None;
    walk(|name, v| {
        if v == value {
            found = Some(name.to_vec());
            false
        } else {
            true
        }
    });
    found
}

/// Whether `setenv(3)` takes `name`: not empty, and no `=` in it.
pub(crate) fn is_settable_name(name: &[u8]) -> bool {
    !name.is_empty() && !name.contains(&b'=')
}

/// `setenv(name, value, 1)`.
///
/// # Panics
///
/// `name` must be one `setenv` takes ([`is_settable_name`]), and neither
/// it nor `value` may hold a NUL.
pub(crate) fn set(name: &[u8], value: &[u8]) {
    let _guard = write_lock();
    // SAFETY: `set_var` is unsafe because libc's environment is not
    // thread-safe. Every reader of ours holds `LOCK`, the standard
    // library's hold `std::env`'s own, and the caller holds the GVL.
    unsafe { std::env::set_var(OsStr::from_bytes(name), OsStr::from_bytes(value)) };
}

/// `unsetenv(name)`. A name `unsetenv` rejects (empty, or with a `=` or
/// a NUL in it) is left alone. `getenv` can still find a variable for
/// one (Darwin's compares only up to the `=`, glibc's finds `A=x` in the
/// entry `A=x=y`); ENV raises `unsetenv`'s `EINVAL` then, as CRuby does
/// (`env_delete_name`).
pub(crate) fn unset(name: &[u8]) {
    if !is_settable_name(name) || name.contains(&0) {
        return;
    }
    let _guard = write_lock();
    // SAFETY: as in `set`.
    unsafe { std::env::remove_var(OsStr::from_bytes(name)) };
}

/// The whole environment copied out into one buffer, for a reader that
/// builds Ruby objects from it or calls a block per variable.
///
/// The copy takes two allocations however many variables there are, and
/// the lock is released before any Ruby code runs — so the block may
/// change the environment, and sees the variables of the moment the
/// snapshot was taken, in `environ`'s order (the order CRuby enumerates
/// in).
pub(crate) struct Snapshot {
    bytes: Vec<u8>,
    /// Per variable: where its name starts, where its value starts and
    /// where its value ends in `bytes`.
    entries: Vec<(u32, u32, u32)>,
}

impl Snapshot {
    pub(crate) fn take() -> Self {
        let mut bytes = Vec::with_capacity(16 * 1024);
        let mut entries = Vec::with_capacity(256);
        walk(|name, value| {
            let start = bytes.len() as u32;
            bytes.extend_from_slice(name);
            let mid = bytes.len() as u32;
            bytes.extend_from_slice(value);
            entries.push((start, mid, bytes.len() as u32));
            true
        });
        Self { bytes, entries }
    }

    pub(crate) fn len(&self) -> usize {
        self.entries.len()
    }

    /// The name and the value of the `i`th variable.
    pub(crate) fn get(&self, i: usize) -> (&[u8], &[u8]) {
        let (start, mid, end) = self.entries[i];
        (
            &self.bytes[start as usize..mid as usize],
            &self.bytes[mid as usize..end as usize],
        )
    }

    pub(crate) fn iter(&self) -> impl Iterator<Item = (&[u8], &[u8])> {
        (0..self.len()).map(|i| self.get(i))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn walk_get_and_snapshot_agree() {
        let snapshot = Snapshot::take();
        assert_eq!(snapshot.len(), snapshot.iter().count());
        assert!(!is_empty());
        // Other tests of this process set and unset variables meanwhile,
        // so only what cannot change is compared: `PATH` is set, and a
        // name with a NUL or an `=` names nothing.
        assert!(snapshot.iter().any(|(name, _)| name == b"PATH"));
        assert!(is_set(b"PATH"));
        assert_eq!(get(b"PATH", |v| v.is_empty()), Some(false));
        assert!(!is_set(b"PA\0TH"));
        assert!(get(b"MONORUBY_ENVIRON_TEST_UNSET", |_| ()).is_none());
        assert!(count() > 0);
        // What an `execve` is handed is that same array.
        let inherited = inherit();
        // SAFETY: the array is NULL-terminated and the lock is held.
        let entries = unsafe {
            let first = inherited.as_ptr();
            (0..).take_while(|&i| !(*first.add(i)).is_null()).count()
        };
        drop(inherited);
        assert!(entries > 0);
        // SAFETY: reading the one entry of the empty array.
        assert!(unsafe { *or_empty(std::ptr::null()) }.is_null());
    }

    #[test]
    fn set_unset_and_long_names() {
        // A name longer than `get`'s stack buffer, and bytes that are
        // not UTF-8 in both halves.
        let mut name = b"MONORUBY_ENVIRON_TEST_".to_vec();
        name.extend(std::iter::repeat_n(b'X', 200));
        name.push(0xff);
        let value = b"v\xfe=w";
        assert!(is_settable_name(&name));
        set(&name, value);
        assert_eq!(get(&name, |v| v.to_vec()).as_deref(), Some(&value[..]));
        assert_eq!(name_of_value(value).as_deref(), Some(&name[..]));
        assert!(
            Snapshot::take()
                .iter()
                .any(|(n, v)| n == name && v == value)
        );
        unset(&name);
        assert!(!is_set(&name));
        assert_eq!(name_of_value(value), None);
        // Names `unsetenv` rejects are left alone.
        assert!(!is_settable_name(b""));
        assert!(!is_settable_name(b"A=B"));
        unset(b"");
        unset(b"A=B");
        unset(b"A\0B");
    }
}
