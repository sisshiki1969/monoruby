use super::*;

//
// `Errno::E*` exception classes.
//
// CRuby builds the `Errno` module dynamically at boot from the system's
// `<errno.h>`, so any errno present on the host (`Errno::ENETDOWN`,
// `Errno::EPROTOTYPE`, …) is reachable, with the host's numbers. monoruby
// mirrors that per-OS via a cfg-gated `ERRNO_TABLE`: the Linux table carries
// the Linux x86_64 numbers, the macOS table the Darwin numbers. So
// `Errno::ENETDOWN::Errno` is 100 on Linux and 50 on macOS — matching CRuby
// on the same OS and the errno the kernel actually reports. Aliases that
// share a number (`EWOULDBLOCK == EAGAIN`, …) are exposed via `ERRNO_ALIASES`
// so the names resolve to the same class object; the alias set is itself
// host-specific (e.g. on macOS `EOPNOTSUPP` and `ENOTSUP` are distinct
// numbers, not aliases).
//
// The hand-rolled `monoruby/builtins/error.rb` previously listed only a
// subset of these and triggered `NameError` for anything unlisted; bundler
// 4's `fetcher/downloader.rb` references `Errno::ENETDOWN`, which crashed
// `require "active_record"` (see issue history).

/// Linux x86_64 errno → exception class name table.
///
/// Numbers are the ABI-stable Linux x86_64 values from
/// `<asm-generic/errno{,-base}.h>` (hard-coded, matching CRuby-on-Linux).
#[cfg(target_os = "linux")]
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

/// macOS / Darwin errno → exception class name table.
///
/// The first block carries the kernel's real errno (1..=106, ABI-stable
/// Darwin numbers). The trailing `_ , 0)` block are names CRuby's `Errno`
/// module still registers on macOS even though the OS does not define them
/// (Linux/BSD errno) — it exposes them as 0-valued placeholder classes, so
/// e.g. `Errno::EDEADLOCK` exists and is distinct from `Errno::EDEADLK`.
/// Mirroring that keeps monoruby's `Errno` byte-identical to CRuby-on-macOS.
/// `EWOULDBLOCK` (== EAGAIN) and `ELAST` (== EQFULL) share a number and are
/// exposed via `ERRNO_ALIASES`.
#[cfg(not(target_os = "linux"))]
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
    ("EDEADLK", 11),
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
    ("EAGAIN", 35),
    ("EINPROGRESS", 36),
    ("EALREADY", 37),
    ("ENOTSOCK", 38),
    ("EDESTADDRREQ", 39),
    ("EMSGSIZE", 40),
    ("EPROTOTYPE", 41),
    ("ENOPROTOOPT", 42),
    ("EPROTONOSUPPORT", 43),
    ("ESOCKTNOSUPPORT", 44),
    ("ENOTSUP", 45),
    ("EPFNOSUPPORT", 46),
    ("EAFNOSUPPORT", 47),
    ("EADDRINUSE", 48),
    ("EADDRNOTAVAIL", 49),
    ("ENETDOWN", 50),
    ("ENETUNREACH", 51),
    ("ENETRESET", 52),
    ("ECONNABORTED", 53),
    ("ECONNRESET", 54),
    ("ENOBUFS", 55),
    ("EISCONN", 56),
    ("ENOTCONN", 57),
    ("ESHUTDOWN", 58),
    ("ETOOMANYREFS", 59),
    ("ETIMEDOUT", 60),
    ("ECONNREFUSED", 61),
    ("ELOOP", 62),
    ("ENAMETOOLONG", 63),
    ("EHOSTDOWN", 64),
    ("EHOSTUNREACH", 65),
    ("ENOTEMPTY", 66),
    ("EPROCLIM", 67),
    ("EUSERS", 68),
    ("EDQUOT", 69),
    ("ESTALE", 70),
    ("EREMOTE", 71),
    ("EBADRPC", 72),
    ("ERPCMISMATCH", 73),
    ("EPROGUNAVAIL", 74),
    ("EPROGMISMATCH", 75),
    ("EPROCUNAVAIL", 76),
    ("ENOLCK", 77),
    ("ENOSYS", 78),
    ("EFTYPE", 79),
    ("EAUTH", 80),
    ("ENEEDAUTH", 81),
    ("EPWROFF", 82),
    ("EDEVERR", 83),
    ("EOVERFLOW", 84),
    ("EBADEXEC", 85),
    ("EBADARCH", 86),
    ("ESHLIBVERS", 87),
    ("EBADMACHO", 88),
    ("ECANCELED", 89),
    ("EIDRM", 90),
    ("ENOMSG", 91),
    ("EILSEQ", 92),
    ("ENOATTR", 93),
    ("EBADMSG", 94),
    ("EMULTIHOP", 95),
    ("ENODATA", 96),
    ("ENOLINK", 97),
    ("ENOSR", 98),
    ("ENOSTR", 99),
    ("EPROTO", 100),
    ("ETIME", 101),
    ("EOPNOTSUPP", 102),
    ("ENOPOLICY", 103),
    ("ENOTRECOVERABLE", 104),
    ("EOWNERDEAD", 105),
    ("EQFULL", 106),
    // Names CRuby registers as 0-valued placeholders on macOS (errno the OS
    // itself does not define). Kept to mirror CRuby's `Errno` constant set.
    ("EADV", 0),
    ("EBADE", 0),
    ("EBADFD", 0),
    ("EBADR", 0),
    ("EBADRQC", 0),
    ("EBADSLT", 0),
    ("EBFONT", 0),
    ("ECAPMODE", 0),
    ("ECHRNG", 0),
    ("ECOMM", 0),
    ("EDEADLOCK", 0),
    ("EDOOFUS", 0),
    ("EDOTDOT", 0),
    ("EHWPOISON", 0),
    ("EIPSEC", 0),
    ("EISNAM", 0),
    ("EKEYEXPIRED", 0),
    ("EKEYREJECTED", 0),
    ("EKEYREVOKED", 0),
    ("EL2HLT", 0),
    ("EL2NSYNC", 0),
    ("EL3HLT", 0),
    ("EL3RST", 0),
    ("ELIBACC", 0),
    ("ELIBBAD", 0),
    ("ELIBEXEC", 0),
    ("ELIBMAX", 0),
    ("ELIBSCN", 0),
    ("ELNRNG", 0),
    ("EMEDIUMTYPE", 0),
    ("ENAVAIL", 0),
    ("ENOANO", 0),
    ("ENOCSI", 0),
    ("ENOKEY", 0),
    ("ENOMEDIUM", 0),
    ("ENONET", 0),
    ("ENOPKG", 0),
    ("ENOTCAPABLE", 0),
    ("ENOTNAM", 0),
    ("ENOTUNIQ", 0),
    ("EREMCHG", 0),
    ("EREMOTEIO", 0),
    ("ERESTART", 0),
    ("ERFKILL", 0),
    ("ESRMNT", 0),
    ("ESTRPIPE", 0),
    ("EUCLEAN", 0),
    ("EUNATCH", 0),
    ("EXFULL", 0),
    ("NOERROR", 0),
];

/// Constant aliases (alias_name → canonical_name): these errno numbers
/// collide on the host, and CRuby exposes both names as the same class
/// object. The set is host-specific (e.g. on macOS `EOPNOTSUPP`/`ENOTSUP`
/// and `EDEADLOCK`/`EDEADLK` are *distinct* numbers, not aliases).
#[cfg(target_os = "linux")]
const ERRNO_ALIASES: &[(&str, &str)] = &[
    ("EWOULDBLOCK", "EAGAIN"),
    ("EDEADLOCK", "EDEADLK"),
    ("EOPNOTSUPP", "ENOTSUP"),
];

#[cfg(not(target_os = "linux"))]
const ERRNO_ALIASES: &[(&str, &str)] = &[
    ("EWOULDBLOCK", "EAGAIN"),
    ("ELAST", "EQFULL"),
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
    // 0 is "no error" — never a real syscall failure. The macOS table maps
    // several host-absent placeholder names to 0, so guard against returning
    // one of those for a 0 query.
    if errno == 0 {
        return None;
    }
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

    // ENETDOWN etc. carry host numbers (100 on Linux, 50 on macOS): the
    // cfg-gated ERRNO_TABLE matches CRuby on each OS, so the `run_test`
    // comparison agrees on both.
    #[test]
    fn errno_classes_dynamically_registered() {
        // bundler 4's `fetcher/downloader.rb` references `Errno::ENETDOWN`
        // (and friends). They must resolve and carry the host `Errno`
        // constant (e.g. ENETDOWN = 100 on Linux, 50 on macOS), which the
        // `run_test` comparison against the host CRuby verifies.
        run_test(
            r#"
            [
              Errno::ENETDOWN.ancestors.include?(SystemCallError),
              Errno::ENETDOWN::Errno,
              Errno::EPROTOTYPE::Errno,
              Errno::EAFNOSUPPORT::Errno,
              Errno::EHOSTDOWN::Errno,
            ]
            "#,
        );
    }

    #[test]
    fn errno_aliases_share_class() {
        // CRuby exposes errno that share a number (`EWOULDBLOCK == EAGAIN`)
        // as the same class object; they must be `equal?`, not just `==`, so
        // existence-of-class checks (`HTTP_RETRYABLE_ERRORS.include?`) work.
        // The `==`-but-not-`equal?` cases differ by host (on macOS
        // EDEADLOCK/EDEADLK and EOPNOTSUPP/ENOTSUP are *distinct* numbers, so
        // not the same class); `run_test` compares against the host CRuby, so
        // monoruby's host-specific alias set must agree on both.
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
