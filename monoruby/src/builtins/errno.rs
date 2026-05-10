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
/// Keep this list in sync with the canonical `Errno.constants` set CRuby
/// produces on Linux x86_64 (entries with a non-zero `Errno` constant).
const ERRNO_TABLE: &[(&str, i32)] = &[
    ("EPERM", libc::EPERM),
    ("ENOENT", libc::ENOENT),
    ("ESRCH", libc::ESRCH),
    ("EINTR", libc::EINTR),
    ("EIO", libc::EIO),
    ("ENXIO", libc::ENXIO),
    ("E2BIG", libc::E2BIG),
    ("ENOEXEC", libc::ENOEXEC),
    ("EBADF", libc::EBADF),
    ("ECHILD", libc::ECHILD),
    ("EAGAIN", libc::EAGAIN),
    ("ENOMEM", libc::ENOMEM),
    ("EACCES", libc::EACCES),
    ("EFAULT", libc::EFAULT),
    ("ENOTBLK", libc::ENOTBLK),
    ("EBUSY", libc::EBUSY),
    ("EEXIST", libc::EEXIST),
    ("EXDEV", libc::EXDEV),
    ("ENODEV", libc::ENODEV),
    ("ENOTDIR", libc::ENOTDIR),
    ("EISDIR", libc::EISDIR),
    ("EINVAL", libc::EINVAL),
    ("ENFILE", libc::ENFILE),
    ("EMFILE", libc::EMFILE),
    ("ENOTTY", libc::ENOTTY),
    ("ETXTBSY", libc::ETXTBSY),
    ("EFBIG", libc::EFBIG),
    ("ENOSPC", libc::ENOSPC),
    ("ESPIPE", libc::ESPIPE),
    ("EROFS", libc::EROFS),
    ("EMLINK", libc::EMLINK),
    ("EPIPE", libc::EPIPE),
    ("EDOM", libc::EDOM),
    ("ERANGE", libc::ERANGE),
    ("EDEADLK", libc::EDEADLK),
    ("ENAMETOOLONG", libc::ENAMETOOLONG),
    ("ENOLCK", libc::ENOLCK),
    ("ENOSYS", libc::ENOSYS),
    ("ENOTEMPTY", libc::ENOTEMPTY),
    ("ELOOP", libc::ELOOP),
    ("ENOMSG", libc::ENOMSG),
    ("EIDRM", libc::EIDRM),
    ("ECHRNG", libc::ECHRNG),
    ("EL2NSYNC", libc::EL2NSYNC),
    ("EL3HLT", libc::EL3HLT),
    ("EL3RST", libc::EL3RST),
    ("ELNRNG", libc::ELNRNG),
    ("EUNATCH", libc::EUNATCH),
    ("ENOCSI", libc::ENOCSI),
    ("EL2HLT", libc::EL2HLT),
    ("EBADE", libc::EBADE),
    ("EBADR", libc::EBADR),
    ("EXFULL", libc::EXFULL),
    ("ENOANO", libc::ENOANO),
    ("EBADRQC", libc::EBADRQC),
    ("EBADSLT", libc::EBADSLT),
    ("EBFONT", libc::EBFONT),
    ("ENOSTR", libc::ENOSTR),
    ("ENODATA", libc::ENODATA),
    ("ETIME", libc::ETIME),
    ("ENOSR", libc::ENOSR),
    ("ENONET", libc::ENONET),
    ("ENOPKG", libc::ENOPKG),
    ("EREMOTE", libc::EREMOTE),
    ("ENOLINK", libc::ENOLINK),
    ("EADV", libc::EADV),
    ("ESRMNT", libc::ESRMNT),
    ("ECOMM", libc::ECOMM),
    ("EPROTO", libc::EPROTO),
    ("EMULTIHOP", libc::EMULTIHOP),
    ("EDOTDOT", libc::EDOTDOT),
    ("EBADMSG", libc::EBADMSG),
    ("EOVERFLOW", libc::EOVERFLOW),
    ("ENOTUNIQ", libc::ENOTUNIQ),
    ("EBADFD", libc::EBADFD),
    ("EREMCHG", libc::EREMCHG),
    ("ELIBACC", libc::ELIBACC),
    ("ELIBBAD", libc::ELIBBAD),
    ("ELIBSCN", libc::ELIBSCN),
    ("ELIBMAX", libc::ELIBMAX),
    ("ELIBEXEC", libc::ELIBEXEC),
    ("EILSEQ", libc::EILSEQ),
    ("ERESTART", libc::ERESTART),
    ("ESTRPIPE", libc::ESTRPIPE),
    ("EUSERS", libc::EUSERS),
    ("ENOTSOCK", libc::ENOTSOCK),
    ("EDESTADDRREQ", libc::EDESTADDRREQ),
    ("EMSGSIZE", libc::EMSGSIZE),
    ("EPROTOTYPE", libc::EPROTOTYPE),
    ("ENOPROTOOPT", libc::ENOPROTOOPT),
    ("EPROTONOSUPPORT", libc::EPROTONOSUPPORT),
    ("ESOCKTNOSUPPORT", libc::ESOCKTNOSUPPORT),
    ("ENOTSUP", libc::ENOTSUP),
    ("EPFNOSUPPORT", libc::EPFNOSUPPORT),
    ("EAFNOSUPPORT", libc::EAFNOSUPPORT),
    ("EADDRINUSE", libc::EADDRINUSE),
    ("EADDRNOTAVAIL", libc::EADDRNOTAVAIL),
    ("ENETDOWN", libc::ENETDOWN),
    ("ENETUNREACH", libc::ENETUNREACH),
    ("ENETRESET", libc::ENETRESET),
    ("ECONNABORTED", libc::ECONNABORTED),
    ("ECONNRESET", libc::ECONNRESET),
    ("ENOBUFS", libc::ENOBUFS),
    ("EISCONN", libc::EISCONN),
    ("ENOTCONN", libc::ENOTCONN),
    ("ESHUTDOWN", libc::ESHUTDOWN),
    ("ETOOMANYREFS", libc::ETOOMANYREFS),
    ("ETIMEDOUT", libc::ETIMEDOUT),
    ("ECONNREFUSED", libc::ECONNREFUSED),
    ("EHOSTDOWN", libc::EHOSTDOWN),
    ("EHOSTUNREACH", libc::EHOSTUNREACH),
    ("EALREADY", libc::EALREADY),
    ("EINPROGRESS", libc::EINPROGRESS),
    ("ESTALE", libc::ESTALE),
    ("EUCLEAN", libc::EUCLEAN),
    ("ENOTNAM", libc::ENOTNAM),
    ("ENAVAIL", libc::ENAVAIL),
    ("EISNAM", libc::EISNAM),
    ("EREMOTEIO", libc::EREMOTEIO),
    ("EDQUOT", libc::EDQUOT),
    ("ENOMEDIUM", libc::ENOMEDIUM),
    ("EMEDIUMTYPE", libc::EMEDIUMTYPE),
    ("ECANCELED", libc::ECANCELED),
    ("ENOKEY", libc::ENOKEY),
    ("EKEYEXPIRED", libc::EKEYEXPIRED),
    ("EKEYREVOKED", libc::EKEYREVOKED),
    ("EKEYREJECTED", libc::EKEYREJECTED),
    ("EOWNERDEAD", libc::EOWNERDEAD),
    ("ENOTRECOVERABLE", libc::ENOTRECOVERABLE),
    ("ERFKILL", libc::ERFKILL),
    ("EHWPOISON", libc::EHWPOISON),
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

    #[test]
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
