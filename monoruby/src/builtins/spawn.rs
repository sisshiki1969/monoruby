//! The CRuby-compatible `Process.spawn` / `Process.exec` engine.
//!
//! Argument parsing (env hash, command forms, exec options) happens
//! here in the parent with full Ruby coercion protocols; everything the
//! child needs after `fork` is prepared up front (CStrings, resolved
//! redirect ops), so the child performs only async-signal-safe syscalls
//! before `execve`. Exec failures are reported back through a
//! `O_CLOEXEC` pipe: the parent reaps the dead child (so `$?` shows the
//! CRuby-compatible exit status 127) and then raises the right
//! `Errno::*` with CRuby's "message - command" text.

use super::*;
use std::ffi::CString;

/// One redirect operation, applied in the child in option order.
pub(crate) struct Redirect {
    /// The child fds being redirected (`[:out, :err] => ...` lists two).
    child_fds: Vec<i32>,
    target: RedirectTarget,
}

enum RedirectTarget {
    /// dup2 from an inherited parent fd. `Fd(n)` with `n == child_fd`
    /// clears the close-on-exec flag instead (CRuby's `fd => fd` form).
    Fd(i32),
    /// Open `path` in the child and dup2 it onto the child fds. The
    /// file is opened once even for multiple child fds.
    File { path: CString, flags: i32, perm: u32 },
    Close,
    /// dup2 from the child's *current* view of another child fd
    /// (`err: [:child, :out]`).
    Child(i32),
}

pub(crate) struct ExecSpec {
    /// Final child environment, "K=V" strings.
    envp: Vec<CString>,
    /// The command word as the user wrote it (for error messages).
    program_display: String,
    /// Candidate program paths: the single path (contains '/'), or one
    /// candidate per PATH dir. Empty means "resolution already failed".
    candidates: Vec<CString>,
    /// argv (argv[0] included).
    argv: Vec<CString>,
    chdir: Option<(CString, String)>,
    umask: Option<libc::mode_t>,
    /// `Some(0)`: new group; `Some(n)`: join group n; `None`: inherit.
    pgroup: Option<i32>,
    redirects: Vec<Redirect>,
    close_others: bool,
}

const STAGE_CHDIR: u8 = b'c';
const STAGE_EXEC: u8 = b'x';

/// Portable CLOEXEC pipe: `pipe2(O_CLOEXEC)` where available (Linux),
/// `pipe` + `fcntl(FD_CLOEXEC)` elsewhere (macOS has no pipe2).
pub(crate) fn pipe_cloexec() -> std::io::Result<(libc::c_int, libc::c_int)> {
    let mut fds: [libc::c_int; 2] = [0; 2];
    // SAFETY: fds is a valid pointer to a 2-element array of c_int.
    #[cfg(target_os = "linux")]
    let rc = unsafe { libc::pipe2(fds.as_mut_ptr(), libc::O_CLOEXEC) };
    #[cfg(not(target_os = "linux"))]
    let rc = unsafe {
        let rc = libc::pipe(fds.as_mut_ptr());
        if rc == 0 {
            libc::fcntl(fds[0], libc::F_SETFD, libc::FD_CLOEXEC);
            libc::fcntl(fds[1], libc::F_SETFD, libc::FD_CLOEXEC);
        }
        rc
    };
    if rc == -1 {
        Err(std::io::Error::last_os_error())
    } else {
        Ok((fds[0], fds[1]))
    }
}

/// Parse `Process.spawn`/`exec` arguments into an `ExecSpec`.
pub(crate) fn parse_spawn_args(
    vm: &mut Executor,
    globals: &mut Globals,
    args: &[Value],
) -> Result<ExecSpec> {
    let null_byte = || MonorubyErr::argumenterr("string contains null byte");
    let mut items: Vec<Value> = args.to_vec();

    // --- env hash (leading) ---
    let mut env_ops: Vec<(String, Option<String>)> = vec![];
    let mut have_env = false;
    if let Some(first) = items.first().copied() {
        let env_hash = if let Some(h) = first.try_hash_ty() {
            Some(h)
        } else if first.is_rstring().is_none()
            && first.try_array_ty().is_none()
            && globals.check_method(first, IdentId::get_id("to_hash")).is_some()
        {
            first.coerce_to_hash(vm, globals).ok()
        } else {
            None
        };
        if let Some(h) = env_hash {
            for (k, v) in h.iter() {
                let key = k.coerce_to_str(vm, globals)?;
                if key.contains('\0') {
                    return Err(null_byte());
                }
                if key.contains('=') {
                    return Err(MonorubyErr::argumenterr(format!(
                        "environment name contains a equal : {key}"
                    )));
                }
                if v.is_nil() {
                    env_ops.push((key, None));
                } else {
                    let val = v.coerce_to_str(vm, globals)?;
                    if val.contains('\0') {
                        return Err(null_byte());
                    }
                    env_ops.push((key, Some(val)));
                }
            }
            have_env = true;
            items.remove(0);
        }
    }

    // --- options hash (trailing) ---
    let mut chdir: Option<(CString, String)> = None;
    let mut umask: Option<libc::mode_t> = None;
    let mut pgroup: Option<i32> = None;
    let mut unsetenv_others = false;
    let mut close_others = false;
    let mut redirects: Vec<Redirect> = vec![];
    if items.len() > 1 || (have_env && items.len() == 1) {
        if let Some(last) = items.last().copied()
            && let Some(opts) = last.try_hash_ty()
        {
            items.pop();
            for (k, v) in opts.iter() {
                if let Some(sym) = k.try_symbol() {
                    match sym.get_name().as_str() {
                        "chdir" => {
                            let rs = v.coerce_to_path_rstring(vm, globals)?;
                            let display =
                                String::from_utf8_lossy(rs.as_bytes()).into_owned();
                            let c = CString::new(rs.as_bytes().to_vec())
                                .map_err(|_| null_byte())?;
                            chdir = Some((c, display));
                        }
                        "umask" => {
                            umask =
                                Some(v.coerce_to_int_i64(vm, globals)? as libc::mode_t);
                        }
                        "pgroup" => {
                            if v.is_nil() || v == Value::bool(false) {
                                pgroup = None;
                            } else if v == Value::bool(true) {
                                pgroup = Some(0);
                            } else if v.try_symbol().is_some() {
                                return Err(MonorubyErr::typeerr(format!(
                                    "wrong argument type {} (expected Integer)",
                                    v.get_real_class_name(&globals.store)
                                )));
                            } else {
                                let n = v.coerce_to_int_i64(vm, globals)?;
                                if n < 0 {
                                    return Err(MonorubyErr::argumenterr(format!(
                                        "negative process group ID : {n}"
                                    )));
                                }
                                pgroup = Some(n as i32);
                            }
                        }
                        "unsetenv_others" | "close_others" => {
                            let flag = if v.is_nil() || v == Value::bool(false) {
                                false
                            } else if v == Value::bool(true) {
                                true
                            } else {
                                return Err(MonorubyErr::argumenterr(format!(
                                    "expected true or false as {sym}: {}",
                                    v.inspect(&globals.store)
                                )));
                            };
                            if sym.get_name() == "unsetenv_others" {
                                unsetenv_others = flag;
                            } else {
                                close_others = flag;
                            }
                        }
                        "in" => redirects.push(parse_redirect(vm, globals, vec![0], v)?),
                        "out" => redirects.push(parse_redirect(vm, globals, vec![1], v)?),
                        "err" => redirects.push(parse_redirect(vm, globals, vec![2], v)?),
                        "exception" => {
                            // Accepted (and consumed) for `system`-style
                            // callers; spawn/exec ignore it.
                        }
                        other => {
                            return Err(MonorubyErr::argumenterr(format!(
                                "wrong exec option symbol: {other}"
                            )));
                        }
                    }
                } else if let Some(fd) = k.try_fixnum() {
                    redirects.push(parse_redirect(vm, globals, vec![fd as i32], v)?);
                } else if k.ty() == Some(ObjTy::IO) {
                    let fd = k.as_io_inner().fileno()?;
                    redirects.push(parse_redirect(vm, globals, vec![fd], v)?);
                } else if let Some(fds) = k.try_array_ty() {
                    let mut child_fds = vec![];
                    for f in fds.iter() {
                        child_fds.push(child_fd_of(globals, *f)?);
                    }
                    redirects.push(parse_redirect(vm, globals, child_fds, v)?);
                } else if k.is_rstring().is_some() {
                    return Err(MonorubyErr::argumenterr(format!(
                        "wrong exec option: {}",
                        k.to_s(&globals.store)
                    )));
                } else {
                    return Err(MonorubyErr::argumenterr(format!(
                        "wrong exec option: {}",
                        k.inspect(&globals.store)
                    )));
                }
            }
        }
    }

    if items.is_empty() {
        return Err(MonorubyErr::argumenterr(
            "wrong number of arguments (given 0, expected 1+)",
        ));
    }

    // --- command ---
    let to_str = |vm: &mut Executor, globals: &mut Globals, v: Value| -> Result<String> {
        let s = v.coerce_to_str(vm, globals)?;
        if s.contains('\0') { Err(null_byte()) } else { Ok(s) }
    };
    // The first command item may be a two-element Array `[command, argv0]`.
    let first = items[0];
    let first_ary = if let Some(a) = first.try_array_ty() {
        Some(a)
    } else if first.is_rstring().is_none()
        && globals.check_method(first, IdentId::get_id("to_ary")).is_some()
    {
        first.coerce_to_array(vm, globals).ok()
    } else {
        None
    };
    let (program, argv_strings, via_shell) = if let Some(ary) = first_ary {
        if ary.len() != 2 {
            return Err(MonorubyErr::argumenterr("wrong first argument"));
        }
        let prog = to_str(vm, globals, ary.get(0).copied().unwrap())?;
        let argv0 = to_str(vm, globals, ary.get(1).copied().unwrap())?;
        let mut argv = vec![argv0];
        for v in items.iter().skip(1) {
            argv.push(to_str(vm, globals, *v)?);
        }
        (prog, argv, false)
    } else if items.len() == 1 {
        let cmd = to_str(vm, globals, first)?;
        let (prog, rest) = crate::builtins::kernel::prepare_command_arg(&cmd);
        // `prepare_command_arg` selected the shell for metacharacter
        // commands: `prog == "sh"` with a leading `-c`.
        let via_shell = prog == "sh" && rest.first().is_some_and(|a| a == "-c");
        let mut argv = vec![prog.clone()];
        argv.extend(rest);
        (prog, argv, via_shell)
    } else {
        let prog = to_str(vm, globals, first)?;
        let mut argv = vec![prog.clone()];
        for v in items.iter().skip(1) {
            argv.push(to_str(vm, globals, *v)?);
        }
        (prog, argv, false)
    };

    // --- final environment ---
    let mut env: Vec<(String, String)> = if unsetenv_others {
        vec![]
    } else {
        std::env::vars().collect()
    };
    for (k, v) in env_ops {
        env.retain(|(ek, _)| ek != &k);
        if let Some(v) = v {
            env.push((k, v));
        }
    }
    let path_env = env
        .iter()
        .find(|(k, _)| k == "PATH")
        .map(|(_, v)| v.clone())
        .unwrap_or_default();
    let envp: Vec<CString> = env
        .into_iter()
        .filter_map(|(k, v)| CString::new(format!("{k}={v}")).ok())
        .collect();

    // --- program path candidates (PATH resolution is finalized by the
    //     child's execve loop; candidates are prepared here so the child
    //     stays allocation-free) ---
    let candidates: Vec<CString> = if via_shell {
        // The shell is exec'd by absolute path so a scrubbed
        // (`unsetenv_others: true`) child environment still works.
        vec![c"/bin/sh".to_owned()]
    } else if program.is_empty() {
        vec![]
    } else if program.contains('/') {
        CString::new(program.clone()).map(|c| vec![c]).unwrap_or_default()
    } else {
        path_env
            .split(':')
            .filter(|d| !d.is_empty())
            .filter_map(|d| CString::new(format!("{d}/{program}")).ok())
            .collect()
    };

    let argv: Vec<CString> = argv_strings
        .into_iter()
        .map(|s| CString::new(s).map_err(|_| null_byte()))
        .collect::<Result<Vec<_>>>()?;

    Ok(ExecSpec {
        envp,
        program_display: program,
        candidates,
        argv,
        chdir,
        umask,
        pgroup,
        redirects,
        close_others,
    })
}

/// Resolve a redirect-key element (`:out`, Integer, IO) to a child fd.
fn child_fd_of(globals: &Globals, v: Value) -> Result<i32> {
    if let Some(sym) = v.try_symbol() {
        match sym.get_name().as_str() {
            "in" => return Ok(0),
            "out" => return Ok(1),
            "err" => return Ok(2),
            _ => {}
        }
    }
    if let Some(i) = v.try_fixnum() {
        return Ok(i as i32);
    }
    if v.ty() == Some(ObjTy::IO) {
        return v.as_io_inner().fileno();
    }
    Err(MonorubyErr::argumenterr(format!(
        "wrong exec redirect: {}",
        v.inspect(&globals.store)
    )))
}

/// Parse a redirect VALUE for the given child fds.
fn parse_redirect(
    vm: &mut Executor,
    globals: &mut Globals,
    child_fds: Vec<i32>,
    v: Value,
) -> Result<Redirect> {
    let null_byte = || MonorubyErr::argumenterr("string contains null byte");
    let default_flags = |child_fds: &[i32]| {
        if child_fds == [0] {
            libc::O_RDONLY
        } else {
            libc::O_WRONLY | libc::O_CREAT | libc::O_TRUNC
        }
    };
    let target = if let Some(sym) = v.try_symbol() {
        match sym.get_name().as_str() {
            "close" => RedirectTarget::Close,
            "in" => RedirectTarget::Fd(0),
            "out" => RedirectTarget::Fd(1),
            "err" => RedirectTarget::Fd(2),
            other => {
                return Err(MonorubyErr::argumenterr(format!(
                    "wrong exec redirect symbol: :{other}"
                )));
            }
        }
    } else if let Some(i) = v.try_fixnum() {
        RedirectTarget::Fd(i as i32)
    } else if v.ty() == Some(ObjTy::IO) {
        RedirectTarget::Fd(v.as_io_inner().fileno()?)
    } else if let Some(s) = v.is_rstring() {
        let path = CString::new(s.as_bytes().to_vec()).map_err(|_| null_byte())?;
        RedirectTarget::File {
            path,
            flags: default_flags(&child_fds),
            perm: 0o644,
        }
    } else if let Some(ary) = v.try_array_ty() {
        // `[:child, fd]`, `[path]`, `[path, mode]`, `[path, mode, perm]`.
        if ary.get(0).and_then(|f| f.try_symbol()).is_some_and(|s| s.get_name() == "child")
        {
            let other = ary
                .get(1)
                .copied()
                .ok_or_else(|| MonorubyErr::argumenterr("wrong exec redirect"))?;
            RedirectTarget::Child(child_fd_of(globals, other)?)
        } else {
            let path_v = ary
                .get(0)
                .copied()
                .ok_or_else(|| MonorubyErr::argumenterr("wrong exec redirect"))?;
            let rs = path_v.coerce_to_path_rstring(vm, globals)?;
            let path = CString::new(rs.as_bytes().to_vec()).map_err(|_| null_byte())?;
            let flags = match ary.get(1).copied() {
                None => default_flags(&child_fds),
                Some(m) => {
                    if let Some(i) = m.try_fixnum() {
                        i as i32
                    } else {
                        let mode = m.coerce_to_str(vm, globals)?;
                        crate::builtins::file::oflags_from_mode_string(&mode)?.0 as i32
                    }
                }
            };
            let perm = match ary.get(2).copied() {
                None => 0o644,
                Some(p) => p.coerce_to_int_i64(vm, globals)? as u32,
            };
            RedirectTarget::File { path, flags, perm }
        }
    } else if globals.check_method(v, IdentId::get_id("to_io")).is_some() {
        let io = vm.invoke_method_inner(globals, IdentId::get_id("to_io"), v, &[], None, None)?;
        if io.ty() != Some(ObjTy::IO) {
            return Err(MonorubyErr::typeerr(format!(
                "can't convert {} to IO ({0}#to_io gives {})",
                v.get_real_class_name(&globals.store),
                io.get_real_class_name(&globals.store)
            )));
        }
        RedirectTarget::Fd(io.as_io_inner().fileno()?)
    } else {
        return Err(MonorubyErr::argumenterr(format!(
            "wrong exec redirect: {}",
            v.inspect(&globals.store)
        )));
    };
    Ok(Redirect { child_fds, target })
}

/// Apply the child-side process attributes and fd plumbing, then
/// `execve` through the candidate list. Only returns on failure, with
/// `(stage, errno)`. Async-signal-safe: no allocation.
///
/// # Safety
/// Must only be called in a freshly forked child (or, for `exec`, on a
/// process that is about to be replaced), single-threaded.
unsafe fn child_exec(spec: &ExecSpec) -> (u8, i32) {
    unsafe {
        // Restore default signal dispositions (monoruby ignores SIGPIPE
        // etc.; a fresh program expects the defaults).
        for sig in 1..32 {
            if sig != libc::SIGKILL && sig != libc::SIGSTOP {
                libc::signal(sig, libc::SIG_DFL);
            }
        }
        if let Some(pg) = spec.pgroup
            && libc::setpgid(0, pg) != 0
        {
            return (STAGE_EXEC, errno());
        }
        if let Some(mask) = spec.umask {
            libc::umask(mask);
        }
        if let Some((dir, _)) = &spec.chdir
            && libc::chdir(dir.as_ptr()) != 0
        {
            return (STAGE_CHDIR, errno());
        }
        for r in &spec.redirects {
            match &r.target {
                RedirectTarget::Fd(src) => {
                    for &cfd in &r.child_fds {
                        if *src == cfd {
                            // `fd => fd`: keep the fd open across exec by
                            // clearing close-on-exec.
                            libc::fcntl(cfd, libc::F_SETFD, 0);
                        } else if libc::dup2(*src, cfd) < 0 {
                            return (STAGE_EXEC, errno());
                        }
                    }
                }
                RedirectTarget::File { path, flags, perm } => {
                    let fd = libc::open(path.as_ptr(), *flags, *perm);
                    if fd < 0 {
                        return (STAGE_EXEC, errno());
                    }
                    for &cfd in &r.child_fds {
                        if fd != cfd && libc::dup2(fd, cfd) < 0 {
                            return (STAGE_EXEC, errno());
                        }
                    }
                    if !r.child_fds.contains(&fd) {
                        libc::close(fd);
                    }
                }
                RedirectTarget::Close => {
                    for &cfd in &r.child_fds {
                        libc::close(cfd);
                    }
                }
                RedirectTarget::Child(other) => {
                    for &cfd in &r.child_fds {
                        if *other != cfd && libc::dup2(*other, cfd) < 0 {
                            return (STAGE_EXEC, errno());
                        }
                    }
                }
            }
        }
        if spec.close_others {
            let max_fd = match libc::sysconf(libc::_SC_OPEN_MAX) {
                n if n > 0 => n as i32,
                _ => 1024,
            };
            for fd in 3..max_fd {
                let redirected = spec
                    .redirects
                    .iter()
                    .any(|r| r.child_fds.contains(&fd));
                if !redirected {
                    libc::close(fd);
                }
            }
        }
        // execve through the candidates. ENOENT candidates continue the
        // PATH walk; an ENOEXEC script (no shebang) is retried via sh.
        let mut argv_ptrs: Vec<*const libc::c_char> =
            spec.argv.iter().map(|a| a.as_ptr()).collect();
        argv_ptrs.push(std::ptr::null());
        let envp_ptrs: Vec<*const libc::c_char> = spec
            .envp
            .iter()
            .map(|e| e.as_ptr())
            .chain(std::iter::once(std::ptr::null()))
            .collect();
        let mut last_errno = libc::ENOENT;
        for cand in &spec.candidates {
            libc::execve(cand.as_ptr(), argv_ptrs.as_ptr(), envp_ptrs.as_ptr());
            let e = errno();
            if e == libc::ENOEXEC {
                // Shell fallback: sh <path> <args...>
                let sh = c"/bin/sh";
                let mut sh_argv: Vec<*const libc::c_char> = vec![sh.as_ptr(), cand.as_ptr()];
                sh_argv.extend(argv_ptrs.iter().skip(1).copied());
                libc::execve(sh.as_ptr(), sh_argv.as_ptr(), envp_ptrs.as_ptr());
                return (STAGE_EXEC, errno());
            }
            // Directories report EACCES like CRuby (Linux execve gives
            // EACCES for directories already; normalize EISDIR anyway).
            let e = if e == libc::EISDIR { libc::EACCES } else { e };
            match e {
                libc::ENOENT | libc::ENOTDIR => {}
                other => {
                    // Only meaningful for multi-candidate PATH walks:
                    // CRuby's own search skips non-executable candidates
                    // and ends with ENOENT.
                    if spec.candidates.len() == 1 {
                        return (STAGE_EXEC, other);
                    }
                    last_errno = libc::ENOENT;
                }
            }
        }
        let _ = last_errno;
        (STAGE_EXEC, libc::ENOENT)
    }
}

fn errno() -> i32 {
    std::io::Error::last_os_error().raw_os_error().unwrap_or(0)
}

/// Store a fresh `Process::Status` for (raw, pid) into `$?`.
fn set_status(vm: &mut Executor, globals: &mut Globals, raw: i32, pid: i32) -> Result<()> {
    let status_class = vm.get_qualified_constant(globals, OBJECT_CLASS, &["Process", "Status"])?;
    let status_obj = vm.invoke_method_inner(
        globals,
        IdentId::NEW,
        status_class,
        &[Value::integer(raw as i64), Value::integer(pid as i64)],
        None,
        None,
    )?;
    crate::scheduler::set_last_status(vm, status_obj);
    Ok(())
}

/// The parent-side error for a reported (stage, errno).
fn stage_error(globals: &Globals, spec: &ExecSpec, stage: u8, errno: i32) -> MonorubyErr {
    let err = std::io::Error::from_raw_os_error(errno);
    match stage {
        STAGE_CHDIR => {
            let dir = spec.chdir.as_ref().map(|(_, d)| d.as_str()).unwrap_or("");
            MonorubyErr::errno_with_msg(&globals.store, &err, dir)
        }
        _ => MonorubyErr::errno_with_msg(&globals.store, &err, &spec.program_display),
    }
}

/// `Process.spawn`: fork + child_exec, reporting exec failure through a
/// CLOEXEC pipe. On failure the child is reaped (so `$?` is the CRuby
/// 127) and the `Errno::*` raised in the parent.
pub(crate) fn do_spawn(
    vm: &mut Executor,
    globals: &mut Globals,
    spec: &ExecSpec,
) -> Result<i64> {
    // CLOEXEC so a successful execve closes the write end and the
    // parent sees EOF.
    let (rfd, wfd) = pipe_cloexec()
        .map_err(|err| MonorubyErr::errno_with_msg(&globals.store, &err, "pipe2"))?;
    // SAFETY: monoruby's threads are green (single OS thread), so the
    // child is single-threaded and consistent; everything it touches was
    // prepared pre-fork.
    let pid = unsafe { libc::fork() };
    if pid < 0 {
        let err = std::io::Error::last_os_error();
        unsafe {
            libc::close(rfd);
            libc::close(wfd);
        }
        return Err(MonorubyErr::errno_with_msg(&globals.store, &err, "fork"));
    }
    if pid == 0 {
        // ===== child =====
        // SAFETY: freshly forked, single-threaded.
        unsafe {
            libc::close(rfd);
            let (stage, e) = child_exec(spec);
            let buf = [stage, (e & 0xff) as u8, ((e >> 8) & 0xff) as u8, ((e >> 16) & 0xff) as u8, ((e >> 24) & 0xff) as u8];
            let _ = libc::write(wfd, buf.as_ptr() as *const libc::c_void, buf.len());
            libc::_exit(127);
        }
    }
    // ===== parent =====
    // SAFETY: plain fd syscalls on fds owned here.
    unsafe {
        libc::close(wfd);
    }
    let mut buf = [0u8; 5];
    let n = loop {
        // SAFETY: reading into a local buffer.
        let n = unsafe { libc::read(rfd, buf.as_mut_ptr() as *mut libc::c_void, buf.len()) };
        if n >= 0 {
            break n;
        }
        let err = std::io::Error::last_os_error();
        if err.raw_os_error() != Some(libc::EINTR) {
            break 0;
        }
        if crate::executor::execute_gc(vm, globals).is_none() {
            unsafe { libc::close(rfd) };
            return Err(vm.take_error());
        }
    };
    // SAFETY: closing our own fd.
    unsafe {
        libc::close(rfd);
    }
    if n >= 5 {
        // Exec failed: reap the child so $? carries exit status 127,
        // then raise.
        let stage = buf[0];
        let e = i32::from_le_bytes([buf[1], buf[2], buf[3], buf[4]]);
        let mut raw: i32 = 0;
        // SAFETY: waitpid on the child we just forked.
        let r = unsafe { libc::waitpid(pid, &mut raw, 0) };
        if r == pid {
            set_status(vm, globals, raw, pid)?;
        }
        return Err(stage_error(globals, spec, stage, e));
    }
    Ok(pid as i64)
}

/// `Process.exec`: apply the spec in-process and `execve`. Only returns
/// on failure (raising the `Errno::*`).
pub(crate) fn do_exec(globals: &mut Globals, spec: &ExecSpec) -> MonorubyErr {
    // SAFETY: exec replaces this process; on failure only fd/process
    // state that CRuby also clobbers has changed.
    let (stage, e) = unsafe { child_exec(spec) };
    stage_error(globals, spec, stage, e)
}

#[cfg(test)]
mod tests {
    use crate::tests::*;

    #[test]
    fn spawn_env_hash() {
        // Leading env Hash sets/unsets child variables; values coerce
        // via #to_str; nil unsets.
        run_test_once(
            r#"
            r, w = IO.pipe
            Process.wait Process.spawn({"SPAWN_T1" => "v1"}, "echo $SPAWN_T1", out: w)
            w.close
            r.read
            "#,
        );
        run_test_once(
            r#"
            ENV["SPAWN_T2"] = "parent"
            r, w = IO.pipe
            Process.wait Process.spawn({"SPAWN_T2" => nil}, "echo [$SPAWN_T2]", out: w)
            w.close
            ENV.delete("SPAWN_T2")
            r.read
            "#,
        );
    }

    #[test]
    fn spawn_env_validation() {
        // '=' or NUL in a key, NUL in a value: ArgumentError before any fork.
        run_test_error(r#"Process.spawn({"A=" => "v"}, "echo")"#);
        run_test_error(r#"Process.spawn({"\0" => "v"}, "echo")"#);
        run_test_error(r#"Process.spawn({"A" => "\0"}, "echo")"#);
    }

    #[test]
    fn spawn_enoent_sets_127() {
        // A nonexistent command raises Errno::ENOENT *and* leaves $? at
        // exit status 127 (the forked child died on the failed exec).
        run_test_once(
            r#"
            begin
              Process.spawn("no-such-cmd-xyzzy")
              :no_raise
            rescue Errno::ENOENT => e
              [e.message, $?.exitstatus]
            end
            "#,
        );
    }

    #[test]
    fn spawn_redirect_forms() {
        // String / [String, mode] / fd targets, and [:out, :err] fan-out.
        run_test_once(
            r##"
            require "tmpdir"
            res = []
            Dir.mktmpdir do |d|
              Process.wait Process.spawn("echo s1", out: "#{d}/o1")
              res << File.read("#{d}/o1")
              Process.wait Process.spawn("echo s2", out: ["#{d}/o2", "w"])
              res << File.read("#{d}/o2")
              File.open("#{d}/o3", "w") do |f|
                Process.wait Process.spawn("echo s3; echo s4 >&2", [:out, :err] => f)
              end
              res << File.read("#{d}/o3")
              File.open("#{d}/o4", "w") do |f|
                Process.wait Process.spawn("echo s5 >&2", :out => f, :err => [:child, :out])
              end
              res << File.read("#{d}/o4")
            end
            res
            "##,
        );
    }

    #[test]
    fn spawn_pgroup_and_umask() {
        run_test_once(
            r#"
            r, w = IO.pipe
            Process.wait Process.spawn("ps -o pgid= -p $$", pgroup: true, out: w)
            w.close
            (r.read.strip.to_i != Process.getpgrp)
            "#,
        );
        run_test_once(
            r#"
            r, w = IO.pipe
            Process.wait Process.spawn("sh -c umask", umask: 0146, out: w)
            w.close
            r.read.strip
            "#,
        );
    }

    #[test]
    fn spawn_chdir() {
        run_test_once(
            r#"
            r, w = IO.pipe
            Process.wait Process.spawn("pwd", chdir: "/", out: w)
            w.close
            r.read
            "#,
        );
        run_test_error(r#"Process.spawn("echo", chdir: "no-such-dir-xyzzy")"#);
    }

    #[test]
    fn spawn_command_array_argv0() {
        run_test_once(
            r#"
            r, w = IO.pipe
            Process.wait Process.spawn(["/bin/sh", "custom_argv0"], "-c", "echo $0", out: w)
            w.close
            r.read
            "#,
        );
        run_test_error(r#"Process.spawn([])"#);
        run_test_error(r#"Process.spawn(["only-one"])"#);
    }

    #[test]
    fn spawn_option_validation() {
        run_test_error(r#"Process.spawn("echo", pgroup: -1)"#);
        run_test_error(r#"Process.spawn("echo", pgroup: :sym)"#);
        run_test_error(r#"Process.spawn("echo", nope_option: 1)"#);
        run_test_error(r#"Process.spawn("echo", "strkey" => 1)"#);
        run_test_error(r#"Process.spawn("echo", close_others: 1)"#);
        run_test_error(r#"Process.spawn({})"#);
        run_test_error(r#"Process.spawn({}, {})"#);
    }

    #[test]
    fn spawn_err_close_and_fd_self() {
        // :err => :close: the child's stderr writes fail (fd closed at
        // exec); sh reports the failed redirect and carries on.
        run_test_once(
            r#"
            r, w = IO.pipe
            pid = Process.spawn("echo out; echo x >&2 || echo rescued", :out => w, :err => :close)
            w.close
            Process.wait(pid)
            r.read
            "#,
        );
        // Integer fd key: dup2 the IO onto a chosen child descriptor.
        // (The `fd => fd` self-redirect form is covered by ruby/spec —
        // "redirects non-default file descriptor to itself" — where fd
        // numbering is stable; under the parallel cargo-test harness fd
        // numbers race between threads.)
        run_test_once(
            r##"
            require "tmpdir"
            Dir.mktmpdir do |d|
              File.open("#{d}/o", "w") do |f|
                pid = Process.spawn("echo bang >&7", 7 => f)
                Process.wait(pid)
              end
              File.read("#{d}/o")
            end
            "##,
        );
    }

    #[test]
    fn exec_validation_before_replacement() {
        // All option validation must complete before the process would be
        // replaced (these must raise in-process, not exec).
        run_test_error(r#"Process.exec("true", unsetenv_others: 1)"#);
        run_test_error(r#"Process.exec("ls\0")"#);
        run_test_error(r#"Process.exec("echo", pgroup: -1)"#);
        // Errno classification happens without replacing the caller.
        run_test_error(r#"Process.exec("no-such-cmd-xyzzy")"#);
        run_test_error(r#"Process.exec("./")"#);
    }
}
