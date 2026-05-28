use super::*;

//
// `Errno::E*` exception classes.
//
// CRuby builds the `Errno` module dynamically at boot from the system's
// `<errno.h>`, so any errno present on the host (`Errno::ENETDOWN`,
// `Errno::EPROTOTYPE`, …) is reachable. monoruby is x86_64 Linux only, so
// we mirror that by walking `libc::E*` at startup and creating
// `Errno::E* < SystemCallError` for each, with the matching `Errno = N`
// constant. Aliases that share an errno number (`EWOULDBLOCK == EAGAIN`,
// `EDEADLOCK == EDEADLK`, `EOPNOTSUPP == ENOTSUP` on Linux) are exposed as
// constant aliases so `Errno::EWOULDBLOCK == Errno::EAGAIN` holds.
//
// The hand-rolled `monoruby/builtins/error.rb` previously listed only a
// subset of these and triggered `NameError` for anything unlisted; bundler
// 4's `fetcher/downloader.rb` references `Errno::ENETDOWN`, which crashed
// `require "active_record"` (see issue history).

/// Linux x86_64 errno → exception class name table.
///
/// Numbers are hard-coded to the Linux x86_64 values from
/// `<asm-generic/errno{,-base}.h>`, *not* looked up through `libc::E*`.
/// The table's contract is to mirror CRuby-on-Linux exactly so that Ruby
/// code (e.g. bundler's `Errno::ENETDOWN == 100`) sees the canonical
/// numbers regardless of the host. This lets the file compile on macOS /
/// other Unixes — needed for the AArch64 backend work where development
/// happens on non-Linux hosts — while still producing the Linux-shaped
/// `Errno` module at runtime. Linux's libc values are ABI-stable kernel
/// constants, so hard-coding does not drift from `libc::E*` on Linux.
const ERRNO_TABLE: &[(&str, i32)] = &[
    ("EPERM", 1),
    ("ENOENT", 2),
    ("ESRCH", 3),
    ("EINTR", 4),
    ("EIO", 5),
    ("ENXIO", 6),
    ("E2BIG", 7),
    ("ENOEXEC", 8),
    ("EBADF", 9),
    ("ECHILD", 10),
    ("EAGAIN", 11),
    ("ENOMEM", 12),
    ("EACCES", 13),
    ("EFAULT", 14),
    ("ENOTBLK", 15),
    ("EBUSY", 16),
    ("EEXIST", 17),
    ("EXDEV", 18),
    ("ENODEV", 19),
    ("ENOTDIR", 20),
    ("EISDIR", 21),
    ("EINVAL", 22),
    ("ENFILE", 23),
    ("EMFILE", 24),
    ("ENOTTY", 25),
    ("ETXTBSY", 26),
    ("EFBIG", 27),
    ("ENOSPC", 28),
    ("ESPIPE", 29),
    ("EROFS", 30),
    ("EMLINK", 31),
    ("EPIPE", 32),
    ("EDOM", 33),
    ("ERANGE", 34),
    ("EDEADLK", 35),
    ("ENAMETOOLONG", 36),
    ("ENOLCK", 37),
    ("ENOSYS", 38),
    ("ENOTEMPTY", 39),
    ("ELOOP", 40),
    ("ENOMSG", 42),
    ("EIDRM", 43),
    ("ECHRNG", 44),
    ("EL2NSYNC", 45),
    ("EL3HLT", 46),
    ("EL3RST", 47),
    ("ELNRNG", 48),
    ("EUNATCH", 49),
    ("ENOCSI", 50),
    ("EL2HLT", 51),
    ("EBADE", 52),
    ("EBADR", 53),
    ("EXFULL", 54),
    ("ENOANO", 55),
    ("EBADRQC", 56),
    ("EBADSLT", 57),
    ("EBFONT", 59),
    ("ENOSTR", 60),
    ("ENODATA", 61),
    ("ETIME", 62),
    ("ENOSR", 63),
    ("ENONET", 64),
    ("ENOPKG", 65),
    ("EREMOTE", 66),
    ("ENOLINK", 67),
    ("EADV", 68),
    ("ESRMNT", 69),
    ("ECOMM", 70),
    ("EPROTO", 71),
    ("EMULTIHOP", 72),
    ("EDOTDOT", 73),
    ("EBADMSG", 74),
    ("EOVERFLOW", 75),
    ("ENOTUNIQ", 76),
    ("EBADFD", 77),
    ("EREMCHG", 78),
    ("ELIBACC", 79),
    ("ELIBBAD", 80),
    ("ELIBSCN", 81),
    ("ELIBMAX", 82),
    ("ELIBEXEC", 83),
    ("EILSEQ", 84),
    ("ERESTART", 85),
    ("ESTRPIPE", 86),
    ("EUSERS", 87),
    ("ENOTSOCK", 88),
    ("EDESTADDRREQ", 89),
    ("EMSGSIZE", 90),
    ("EPROTOTYPE", 91),
    ("ENOPROTOOPT", 92),
    ("EPROTONOSUPPORT", 93),
    ("ESOCKTNOSUPPORT", 94),
    // EOPNOTSUPP is a Linux alias of ENOTSUP (both 95); exposed via ERRNO_ALIASES.
    ("ENOTSUP", 95),
    ("EPFNOSUPPORT", 96),
    ("EAFNOSUPPORT", 97),
    ("EADDRINUSE", 98),
    ("EADDRNOTAVAIL", 99),
    ("ENETDOWN", 100),
    ("ENETUNREACH", 101),
    ("ENETRESET", 102),
    ("ECONNABORTED", 103),
    ("ECONNRESET", 104),
    ("ENOBUFS", 105),
    ("EISCONN", 106),
    ("ENOTCONN", 107),
    ("ESHUTDOWN", 108),
    ("ETOOMANYREFS", 109),
    ("ETIMEDOUT", 110),
    ("ECONNREFUSED", 111),
    ("EHOSTDOWN", 112),
    ("EHOSTUNREACH", 113),
    ("EALREADY", 114),
    ("EINPROGRESS", 115),
    ("ESTALE", 116),
    ("EUCLEAN", 117),
    ("ENOTNAM", 118),
    ("ENAVAIL", 119),
    ("EISNAM", 120),
    ("EREMOTEIO", 121),
    ("EDQUOT", 122),
    ("ENOMEDIUM", 123),
    ("EMEDIUMTYPE", 124),
    ("ECANCELED", 125),
    ("ENOKEY", 126),
    ("EKEYEXPIRED", 127),
    ("EKEYREVOKED", 128),
    ("EKEYREJECTED", 129),
    ("EOWNERDEAD", 130),
    ("ENOTRECOVERABLE", 131),
    ("ERFKILL", 132),
    ("EHWPOISON", 133),
];

/// Constant aliases (alias_name → canonical_name): on Linux these errno
/// numbers collide, and CRuby exposes both names as the same class object.
const ERRNO_ALIASES: &[(&str, &str)] = &[
    ("EWOULDBLOCK", "EAGAIN"),
    ("EDEADLOCK", "EDEADLK"),
    ("EOPNOTSUPP", "ENOTSUP"),
];

pub(super) fn init(globals: &mut Globals) {
    let object = globals.object_class();
    let syscall_err = globals
        .store
        .get_constant_noautoload(OBJECT_CLASS, IdentId::get_id("SystemCallError"))
        .expect("SystemCallError must be defined before errno::init");
    let syscall_err_module = syscall_err.expect_class(globals).unwrap();

    // `Errno` is a plain class containing the per-errno exception classes,
    // matching CRuby (which exposes it as a module — we use a class here for
    // continuity with the existing `monoruby/builtins/error.rb`, which the
    // Ruby side reopens after this).
    let errno = globals.define_class("Errno", object, OBJECT_CLASS);
    let errno_id = errno.id();

    for (name, num) in ERRNO_TABLE {
        let cls = globals.define_class(name, syscall_err_module, errno_id);
        globals.set_constant_by_str(cls.id(), "Errno", Value::integer(*num as i64));
    }
    for (alias, canonical) in ERRNO_ALIASES {
        let target = globals
            .store
            .get_constant_noautoload(errno_id, IdentId::get_id(canonical))
            .unwrap_or_else(|| panic!("Errno alias target {canonical} not defined"));
        globals.set_constant_by_str(errno_id, alias, target);
    }
}

/// Look up the canonical Ruby `Errno` class name (e.g. `"ENETDOWN"`) for a
/// raw OS errno number. Returns `None` for unknown values.
pub(crate) fn errno_to_name(errno: i32) -> Option<&'static str> {
    ERRNO_TABLE
        .iter()
        .find_map(|(name, n)| if *n == errno { Some(*name) } else { None })
}

/// Get the system's `strerror` text for an errno number. Falls back to
/// `"Unknown error <N>"` if `strerror` returns NULL.
///
/// Single-threaded use only: monoruby's runtime is single-threaded so the
/// non-`_r` form is safe; the returned string is copied immediately.
pub(crate) fn strerror(errno: i32) -> String {
    // SAFETY: `libc::strerror` returns a pointer to a static or
    // thread-local buffer that's valid until the next `strerror` call on
    // the same thread; we copy it out via `to_string_lossy` before
    // returning. monoruby is single-threaded so there are no cross-thread
    // races on the buffer.
    unsafe {
        let ptr = libc::strerror(errno);
        if ptr.is_null() {
            format!("Unknown error {errno}")
        } else {
            std::ffi::CStr::from_ptr(ptr)
                .to_string_lossy()
                .into_owned()
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::tests::*;

    // macOS uses different errno numbers than Linux (e.g. ENETDOWN is 50 vs 100).
    // monoruby intentionally exposes the Linux numbers (see ERRNO_TABLE comment),
    // so the system-CRuby comparison in `run_test` doesn't agree on macOS. Skip
    // on macOS; the production behaviour (Linux numbers everywhere) is still
    // covered on Linux CI.
    #[test]
    #[cfg_attr(
        target_os = "macos",
        ignore = "Linux-mirrored errno numbers diverge from CRuby on macOS"
    )]
    fn errno_classes_dynamically_registered() {
        // bundler 4's `fetcher/downloader.rb` references `Errno::ENETDOWN`
        // (and friends). They must resolve and carry the right `Errno`
        // constant, with the canonical Linux x86_64 numbers.
        run_test(
            r#"
            [
              Errno::ENETDOWN.ancestors.include?(SystemCallError),
              Errno::ENETDOWN::Errno,           # 100
              Errno::EPROTOTYPE::Errno,         # 91
              Errno::EAFNOSUPPORT::Errno,       # 97
              Errno::EHOSTDOWN::Errno,          # 112
            ]
            "#,
        );
    }

    #[test]
    #[cfg_attr(
        target_os = "macos",
        ignore = "Linux EAGAIN/EWOULDBLOCK aliasing differs from macOS's distinct numbers"
    )]
    fn errno_aliases_share_class() {
        // CRuby exposes `EWOULDBLOCK == EAGAIN` etc. as the same class
        // object; they must be `equal?`, not just `==`, so existence-of-
        // class checks (`HTTP_RETRYABLE_ERRORS.include?`) work right.
        run_test(
            r#"
            [
              Errno::EWOULDBLOCK.equal?(Errno::EAGAIN),
              Errno::EDEADLOCK.equal?(Errno::EDEADLK),
              Errno::EOPNOTSUPP.equal?(Errno::ENOTSUP),
            ]
            "#,
        );
    }

    #[test]
    fn errno_message_format() {
        // `Errno::E*.new(arg)` should yield CRuby's "<strerror> - <arg>"
        // shape. The descriptive prefix comes from the per-class
        // `initialize` overrides in `monoruby/builtins/error.rb` (which
        // mirrors CRuby's hard-coded strings, not the host's strerror,
        // for portability of test output).
        run_test(
            r#"
            [
              Errno::ENOENT.new("foo").message,
              Errno::EACCES.new("bar").message,
              Errno::EEXIST.new.message,
            ]
            "#,
        );
    }
}
