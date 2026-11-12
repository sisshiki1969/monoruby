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
    /// `__pty_session: true` (stdlib/pty.rb's `PTY.spawn`, not a CRuby
    /// option): the child starts a new session and takes the terminal on
    /// its stdin, which the redirects have made a pty's slave, as its
    /// controlling terminal.
    pty_session: bool,
}

impl ExecSpec {
    /// The command word as the user wrote it, for messages (CRuby names
    /// only the program in `Command failed with ...`).
    pub(crate) fn program_display(&self) -> &str {
        &self.program_display
    }

    /// The parent's fd the child's stdin is taken from, for a
    /// `__pty_session` spawn: the pty's slave.
    fn pty_slave(&self) -> Option<i32> {
        if !self.pty_session {
            return None;
        }
        self.redirects.iter().find_map(|r| match r.target {
            RedirectTarget::Fd(fd) if r.child_fds.contains(&0) => Some(fd),
            _ => None,
        })
    }
}

/// Drop the parent's reference to a pty's slave as soon as the child is
/// forked, as CRuby's ext/pty closes its copy right after its fork.
///
/// The last close of a slave waits for the output to drain and then
/// flushes what the master has not read (macOS), so the slave must be
/// the child's to close last: a parent that still held it when a quick
/// child (`PTY.spawn("printf", ...)`) had exited would close it last
/// and lose the child's output. `stdlib/pty.rb` still owns the fd
/// number and closes it afterwards, so it is pointed at `/dev/null`
/// rather than closed here.
///
/// # Safety
///
/// `fd` must be an fd this process owns.
unsafe fn release_pty_slave(fd: i32) {
    // SAFETY: plain fd syscalls; `fd` is ours (caller's contract) and
    // `null` is a fresh descriptor closed again below. Should the open
    // fail, `dup2` / `close` of -1 are harmless EBADFs and the slave
    // stays as it was.
    unsafe {
        let null = libc::open(c"/dev/null".as_ptr(), libc::O_RDWR | libc::O_CLOEXEC);
        libc::dup2(null, fd);
        libc::close(null);
    }
}

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
    parse_exec_args(vm, globals, args, true)
}

/// [`parse_spawn_args`], with CRuby's `accept_shell`: `false` (the
/// Array form of `IO.popen`) runs a lone command word as a program even
/// when it holds shell metacharacters.
pub(crate) fn parse_exec_args(
    vm: &mut Executor,
    globals: &mut Globals,
    args: &[Value],
    accept_shell: bool,
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
    let mut pty_session = false;
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
                            umask = Some(num2uint(vm, globals, v)? as libc::mode_t);
                        }
                        "pgroup" => {
                            if v.is_nil() || v == Value::bool(false) {
                                pgroup = None;
                            } else if v == Value::bool(true) {
                                pgroup = Some(0);
                            } else if v.try_symbol().is_some() {
                                return Err(MonorubyErr::typeerr(format!(
                                    "wrong argument type {} (expected Integer)",
                                    v.builtin_class_name(&globals.store)
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
                        "__pty_session" => pty_session = v.as_bool(),
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
    // The words go to the child as bytes: an argv entry may hold bytes
    // that are not UTF-8 (a `# encoding: big5` script passed via `-e`).
    let to_str = |vm: &mut Executor, globals: &mut Globals, v: Value| -> Result<Vec<u8>> {
        let s = v.coerce_to_rstring(vm, globals)?.as_bytes().to_vec();
        if s.contains(&0) {
            Err(null_byte())
        } else {
            Ok(s)
        }
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
    } else if items.len() == 1 && accept_shell {
        let cmd = first.coerce_to_str(vm, globals)?;
        if cmd.contains('\0') {
            return Err(null_byte());
        }
        let (prog, rest) = crate::builtins::kernel::prepare_command_arg(&cmd);
        // `prepare_command_arg` selected the shell for metacharacter
        // commands: `prog == "sh"` with a leading `-c`.
        let via_shell = prog == "sh" && rest.first().is_some_and(|a| a == "-c");
        let mut argv = vec![prog.clone().into_bytes()];
        argv.extend(rest.into_iter().map(String::into_bytes));
        (prog.into_bytes(), argv, via_shell)
    } else {
        let prog = to_str(vm, globals, first)?;
        let mut argv = vec![prog.clone()];
        for v in items.iter().skip(1) {
            argv.push(to_str(vm, globals, *v)?);
        }
        (prog, argv, false)
    };

    // The program is looked up along the env Hash's `PATH` when it gives
    // one, and the parent's otherwise — `unsetenv_others:` clears the
    // child's environment, not the search (CRuby's `path_env`).
    let path_env = env_ops
        .iter()
        .rev()
        .find(|(k, _)| k == "PATH")
        .and_then(|(_, v)| v.clone())
        .unwrap_or_else(|| std::env::var("PATH").unwrap_or_default());
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
    } else if program.contains(&b'/') {
        CString::new(program.clone())
            .map(|c| vec![c])
            .unwrap_or_default()
    } else {
        path_env
            .split(':')
            .filter(|d| !d.is_empty())
            .filter_map(|d| {
                let mut path = format!("{d}/").into_bytes();
                path.extend_from_slice(&program);
                CString::new(path).ok()
            })
            .collect()
    };

    let argv: Vec<CString> = argv_strings
        .into_iter()
        .map(|s| CString::new(s).map_err(|_| null_byte()))
        .collect::<Result<Vec<_>>>()?;

    Ok(ExecSpec {
        envp,
        program_display: String::from_utf8_lossy(&program).into_owned(),
        candidates,
        argv,
        chdir,
        umask,
        pgroup,
        redirects,
        close_others,
        pty_session,
    })
}

/// Whether `k` is a key CRuby's `rb_execarg_addopt` takes as an exec
/// option — what `IO.popen` hands the child out of its options Hash
/// (`rb_execarg_extract_options`), leaving the rest to the stream.
pub(crate) fn is_exec_option_key(k: Value) -> bool {
    if let Some(sym) = k.try_symbol() {
        let name = sym.get_name();
        return matches!(
            name.as_str(),
            "chdir"
                | "umask"
                | "pgroup"
                | "new_pgroup"
                | "unsetenv_others"
                | "close_others"
                | "in"
                | "out"
                | "err"
                | "uid"
                | "gid"
                | "exception"
        ) || name.starts_with("rlimit_");
    }
    k.try_fixnum().is_some() || k.ty() == Some(ObjTy::IO) || k.try_array_ty().is_some()
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
                    "wrong exec redirect symbol: {other}"
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
                // CRuby: the Array form WITHOUT a mode defaults to
                // read-only regardless of the target fd (unlike the bare
                // String form, whose default depends on the fd).
                None => libc::O_RDONLY,
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

/// One entry of CRuby's `fd_dup2` list (`run_exec_dup2`'s
/// `run_exec_dup2_fd_pair`): `dup2(oldfd, newfd)` in the child, ordered so
/// that no pair overwrites a descriptor another pair has yet to read.
#[derive(Clone, Copy)]
struct Dup2Pair {
    oldfd: i32,
    newfd: i32,
    /// The pair whose `newfd` is this pair's `oldfd` (it must wait for
    /// this one), or -1.
    older_index: isize,
    /// How many pairs still read this pair's `newfd`.
    num_newer: usize,
}

/// The child's fd plumbing, prepared in the parent as CRuby's
/// `rb_execarg_parent_start1` prepares it: the files a redirect names are
/// opened *here*, so a path that cannot be opened is reported by name
/// before anything forks ("No such file or directory - /x"), relative to
/// the parent's directory; the redirects are checked ("fd 1 specified
/// twice", "child fd 1 is not redirected"), and sorted into the three
/// lists `rb_execarg_run_options` performs in order — every `dup2`, then
/// every `:close`, then every `[:child, fd]`.
pub(crate) struct ChildFds {
    /// `fd_dup2`, sorted by `oldfd`, with `run_exec_dup2`'s bookkeeping
    /// (the child works on its own copy of it).
    dup2: Vec<Dup2Pair>,
    /// `fd_close`.
    close: Vec<i32>,
    /// `fd_dup2_child` as `(newfd, oldfd)`, `oldfd` resolved through
    /// chains of `[:child, fd]`.
    dup2_child: Vec<(i32, i32)>,
    /// Every child fd a redirect names (CRuby's `redirect_fds`), which
    /// `close_others:` leaves alone.
    redirected: Vec<i32>,
    /// The parent's descriptors for the opened files (close-on-exec).
    /// Closed when this is dropped, after the fork.
    opened: Vec<i32>,
}

impl Drop for ChildFds {
    fn drop(&mut self) {
        for &fd in &self.opened {
            // SAFETY: fds this struct opened and nothing else owns.
            unsafe { libc::close(fd) };
        }
    }
}

impl ChildFds {
    /// The highest descriptor any redirect reads, writes or closes.
    fn max_fd(&self) -> i32 {
        let pairs = self.dup2.iter().flat_map(|p| [p.oldfd, p.newfd]);
        let child = self.dup2_child.iter().flat_map(|&(n, o)| [n, o]);
        pairs
            .chain(child)
            .chain(self.close.iter().copied())
            .max()
            .unwrap_or(2)
    }
}

/// Open `spec`'s file redirects and check and order all of its
/// redirects (see [`ChildFds`]). `extra_dup2` are `(newfd, oldfd)` pairs
/// the caller adds itself — `IO.popen`'s pipe ends — which take part in
/// the checks as CRuby's `rb_execarg_addopt` entries do.
pub(crate) fn prepare_child_fds(
    vm: &mut Executor,
    globals: &mut Globals,
    spec: &ExecSpec,
    extra_dup2: &[(i32, i32)],
) -> Result<ChildFds> {
    let mut fds = ChildFds {
        dup2: vec![],
        close: vec![],
        dup2_child: vec![],
        redirected: vec![],
        opened: vec![],
    };
    // `(newfd, oldfd)` in CRuby's list order: the fd redirects as given,
    // the caller's, then the opened files.
    let mut dup2: Vec<(i32, i32)> = vec![];
    for r in &spec.redirects {
        match &r.target {
            RedirectTarget::Fd(src) => dup2.extend(r.child_fds.iter().map(|&c| (c, *src))),
            RedirectTarget::Close => fds.close.extend(&r.child_fds),
            RedirectTarget::Child(other) => fds
                .dup2_child
                .extend(r.child_fds.iter().map(|&c| (c, *other))),
            RedirectTarget::File { .. } => {}
        }
    }
    dup2.extend_from_slice(extra_dup2);
    for r in &spec.redirects {
        if let RedirectTarget::File { path, flags, perm } = &r.target {
            let fd = open_in_parent(vm, globals, path, *flags, *perm)?;
            fds.opened.push(fd);
            dup2.extend(r.child_fds.iter().map(|&c| (c, fd)));
        }
    }
    check_exec_fds(&mut fds, &dup2)?;
    let mut pairs: Vec<Dup2Pair> = dup2
        .iter()
        .map(|&(newfd, oldfd)| Dup2Pair {
            oldfd,
            newfd,
            older_index: -1,
            num_newer: 0,
        })
        .collect();
    pairs.sort_by_key(|p| p.oldfd);
    for i in 0..pairs.len() {
        let newfd = pairs[i].newfd;
        for k in 0..pairs.len() {
            if pairs[k].oldfd == newfd {
                pairs[i].num_newer += 1;
                pairs[k].older_index = i as isize;
            }
        }
    }
    fds.dup2 = pairs;
    Ok(fds)
}

/// Open a redirect's file for the child, close-on-exec, outside the GVL
/// (a FIFO's open blocks until the other end appears), as CRuby's
/// `open_func`. A failure names the path.
fn open_in_parent(
    vm: &mut Executor,
    globals: &mut Globals,
    path: &CString,
    flags: i32,
    perm: u32,
) -> Result<i32> {
    loop {
        let ptr = path.as_ptr() as usize;
        let res = crate::scheduler::without_gvl(vm, move || {
            // SAFETY: `ptr` is the NUL-terminated path, alive for the call.
            let fd =
                unsafe { libc::open(ptr as *const libc::c_char, flags | libc::O_CLOEXEC, perm) };
            if fd < 0 { Err(errno()) } else { Ok(fd) }
        });
        match res {
            Ok(fd) => return Ok(fd),
            Err(libc::EINTR) => {
                if crate::executor::execute_gc(vm, globals).is_none() {
                    return Err(vm.take_error());
                }
            }
            Err(e) => {
                use std::os::unix::ffi::OsStrExt;
                let err = std::io::Error::from_raw_os_error(e);
                let path = std::ffi::OsStr::from_bytes(path.as_bytes());
                return Err(MonorubyErr::errno_with_msg(&globals.store, &err, path));
            }
        }
    }
}

/// CRuby's `check_exec_fds`: no child fd may be named twice, and a
/// `[:child, fd]` must end, through any chain of them, at an fd another
/// redirect `dup2`s — which it is rewritten to name.
fn check_exec_fds(fds: &mut ChildFds, dup2: &[(i32, i32)]) -> Result<()> {
    /// `Some(None)` is CRuby's `Qtrue` (a `dup2` target), `Some(Some(n))`
    /// a `[:child, n]` (or `-1`, a `:close`).
    fn lookup(h: &[(i32, Option<i32>)], fd: i32) -> Option<Option<i32>> {
        h.iter().find(|(k, _)| *k == fd).map(|(_, v)| *v)
    }
    fn set(h: &mut Vec<(i32, Option<i32>)>, fd: i32, v: Option<i32>) {
        match h.iter_mut().find(|(k, _)| *k == fd) {
            Some(e) => e.1 = v,
            None => h.push((fd, v)),
        }
    }
    let mut h: Vec<(i32, Option<i32>)> = vec![];
    let entries = dup2
        .iter()
        .map(|&(n, _)| (n, None))
        .chain(fds.close.iter().map(|&n| (n, Some(-1))))
        .chain(fds.dup2_child.iter().map(|&(n, o)| (n, Some(o))));
    for (fd, v) in entries {
        if lookup(&h, fd).is_some() {
            return Err(MonorubyErr::argumenterr(format!("fd {fd} specified twice")));
        }
        h.push((fd, v));
    }
    let len = fds.dup2_child.len();
    for i in 0..len {
        let (newfd, oldfd) = fds.dup2_child[i];
        let mut lastfd = oldfd;
        let mut val = lookup(&h, lastfd);
        let mut depth = 0;
        while let Some(Some(next)) = val
            && next >= 0
        {
            lastfd = next;
            val = lookup(&h, next);
            if len < depth {
                return Err(MonorubyErr::argumenterr(format!(
                    "cyclic child fd redirection from {oldfd}"
                )));
            }
            depth += 1;
        }
        if val != Some(None) {
            return Err(MonorubyErr::argumenterr(format!(
                "child fd {oldfd} is not redirected"
            )));
        }
        if oldfd != lastfd {
            fds.dup2_child[i].1 = lastfd;
            set(&mut h, newfd, Some(lastfd));
            let mut v = oldfd;
            while let Some(Some(v2)) = lookup(&h, v) {
                set(&mut h, v, Some(lastfd));
                v = v2;
            }
        }
    }
    fds.redirected = h.into_iter().map(|(k, _)| k).collect();
    Ok(())
}

/// The call that failed in the child, reported back with its errno. All
/// but `STAGE_EXEC` are named in the parent's error, as CRuby's `ERRMSG`.
const STAGE_EXEC: u8 = 0;
const STAGE_CHDIR: u8 = 1;
const STAGE_DUP2: u8 = 2;
const STAGE_DUP: u8 = 3;
const STAGE_CLOSE: u8 = 4;
const STAGE_SETPGID: u8 = 5;
const STAGE_GETFD: u8 = 6;
const STAGE_SETFD: u8 = 7;

fn stage_name(stage: u8) -> &'static str {
    match stage {
        STAGE_CHDIR => "chdir",
        STAGE_DUP2 => "dup2",
        STAGE_DUP => "dup",
        STAGE_CLOSE => "close",
        STAGE_SETPGID => "setpgid",
        STAGE_GETFD => "fcntl(F_GETFD)",
        STAGE_SETFD => "fcntl(F_SETFD)",
        _ => "",
    }
}

/// CRuby's `fd_clear_cloexec`.
///
/// # Safety
/// Plain fcntl calls; async-signal-safe.
unsafe fn clear_cloexec(fd: i32) -> std::result::Result<(), (u8, i32)> {
    // SAFETY: caller's contract.
    unsafe {
        let flags = libc::fcntl(fd, libc::F_GETFD);
        if flags == -1 {
            return Err((STAGE_GETFD, errno()));
        }
        if flags & libc::FD_CLOEXEC != 0
            && libc::fcntl(fd, libc::F_SETFD, flags & !libc::FD_CLOEXEC) == -1
        {
            return Err((STAGE_SETFD, errno()));
        }
    }
    Ok(())
}

/// CRuby's `run_exec_dup2`: perform every `dup2` pair, in an order that
/// never overwrites a descriptor a later pair still reads, breaking
/// cycles (`out: :err, err: :out`) through one spare descriptor. A pair
/// that names the same fd twice (`7 => 7`) clears its close-on-exec flag.
/// Async-signal-safe: works on `pairs` in place, no allocation.
///
/// # Safety
/// Must only be called in a freshly forked child (or a process about to
/// exec).
unsafe fn run_exec_dup2(pairs: &mut [Dup2Pair]) -> std::result::Result<(), (u8, i32)> {
    // SAFETY: fd syscalls in the child (caller's contract).
    unsafe {
        let n = pairs.len();
        // Non-cyclic redirections.
        for i in 0..n {
            let mut j = i as isize;
            while j != -1 && pairs[j as usize].oldfd != -1 && pairs[j as usize].num_newer == 0 {
                let p = pairs[j as usize];
                if libc::dup2(p.oldfd, p.newfd) == -1 {
                    return Err((STAGE_DUP2, errno()));
                }
                pairs[j as usize].oldfd = -1;
                j = p.older_index;
                if j != -1 {
                    pairs[j as usize].num_newer -= 1;
                }
            }
        }
        // Cyclic redirections.
        let mut extra_fd = -1;
        for i in 0..n {
            if pairs[i].oldfd == -1 {
                continue;
            }
            if pairs[i].oldfd == pairs[i].newfd {
                clear_cloexec(pairs[i].oldfd)?;
                pairs[i].oldfd = -1;
                continue;
            }
            if extra_fd == -1 {
                extra_fd = libc::dup(pairs[i].oldfd);
                if extra_fd == -1 {
                    return Err((STAGE_DUP, errno()));
                }
            } else if libc::dup2(pairs[i].oldfd, extra_fd) == -1 {
                return Err((STAGE_DUP2, errno()));
            }
            pairs[i].oldfd = extra_fd;
            let mut j = pairs[i].older_index;
            pairs[i].older_index = -1;
            while j != -1 {
                let p = pairs[j as usize];
                if libc::dup2(p.oldfd, p.newfd) == -1 {
                    return Err((STAGE_DUP2, errno()));
                }
                pairs[j as usize].oldfd = -1;
                j = p.older_index;
            }
        }
        if extra_fd != -1 && libc::close(extra_fd) == -1 {
            return Err((STAGE_CLOSE, errno()));
        }
    }
    Ok(())
}

/// Apply the child-side process attributes and fd plumbing in CRuby's
/// order (`rb_execarg_run_options`: pgroup, umask, the `dup2`s, the
/// closes, `close_others`, the `[:child, fd]`s, chdir), then `execve`
/// through the candidate list. Only returns on failure, with
/// `(stage, errno)`. Async-signal-safe: no allocation.
///
/// # Safety
/// Must only be called in a freshly forked child (or, for `exec`, on a
/// process that is about to be replaced), single-threaded.
unsafe fn child_exec(spec: &ExecSpec, fds: &mut ChildFds, ptrs: &mut ExecPtrs) -> (u8, i32) {
    unsafe {
        // Restore default signal dispositions (monoruby ignores SIGPIPE
        // etc.; a fresh program expects the defaults).
        for sig in 1..32 {
            if sig != libc::SIGKILL && sig != libc::SIGSTOP {
                libc::signal(sig, libc::SIG_DFL);
            }
        }
        // ... and the signal mask: a non-main kernel thread of the 1:1
        // model blocks the asynchronous signals, and the child would
        // inherit that (`scheduler::command_for_child`).
        crate::scheduler::reset_child_sigmask();
        if let Some(pg) = spec.pgroup
            && libc::setpgid(0, pg) != 0
        {
            return (STAGE_SETPGID, errno());
        }
        if let Some(mask) = spec.umask {
            libc::umask(mask);
        }
        if let Err(e) = run_exec_dup2(&mut fds.dup2) {
            return e;
        }
        for &fd in &fds.close {
            if libc::close(fd) == -1 {
                return (STAGE_CLOSE, errno());
            }
        }
        if spec.close_others {
            // CRuby's `rb_close_before_exec`: everything above stderr that
            // no redirect names is marked close-on-exec.
            let max_fd = match libc::sysconf(libc::_SC_OPEN_MAX) {
                n if n > 0 => n as i32,
                _ => 1024,
            };
            for fd in 3..max_fd {
                if !fds.redirected.contains(&fd) {
                    let flags = libc::fcntl(fd, libc::F_GETFD);
                    if flags != -1 && flags & libc::FD_CLOEXEC == 0 {
                        libc::fcntl(fd, libc::F_SETFD, flags | libc::FD_CLOEXEC);
                    }
                }
            }
        }
        for &(newfd, oldfd) in &fds.dup2_child {
            if libc::dup2(oldfd, newfd) == -1 {
                return (STAGE_DUP2, errno());
            }
        }
        if spec.pty_session {
            libc::setsid();
            if libc::ioctl(0, libc::TIOCSCTTY as _, 0) != 0 {
                return (STAGE_EXEC, errno());
            }
        }
        if let Some((dir, _)) = &spec.chdir
            && libc::chdir(dir.as_ptr()) != 0
        {
            return (STAGE_CHDIR, errno());
        }
        // execve through the candidates. ENOENT candidates continue the
        // PATH walk; an ENOEXEC script (no shebang) is retried via sh.
        for cand in &spec.candidates {
            libc::execve(cand.as_ptr(), ptrs.argv.as_ptr(), ptrs.envp.as_ptr());
            let e = errno();
            if e == libc::ENOEXEC {
                // Shell fallback: sh <path> <args...>
                ptrs.sh_argv[1] = cand.as_ptr();
                libc::execve(
                    c"/bin/sh".as_ptr(),
                    ptrs.sh_argv.as_ptr(),
                    ptrs.envp.as_ptr(),
                );
                return (STAGE_EXEC, errno());
            }
            // Directories report EACCES like CRuby (Linux execve gives
            // EACCES for directories already; normalize EISDIR anyway).
            let e = if e == libc::EISDIR { libc::EACCES } else { e };
            // ENOENT / ENOTDIR continue the PATH walk. Anything else ends
            // a single-candidate exec; in a multi-candidate walk CRuby's
            // own search skips non-executable candidates and ends with
            // ENOENT.
            if !matches!(e, libc::ENOENT | libc::ENOTDIR) && spec.candidates.len() == 1 {
                return (STAGE_EXEC, e);
            }
        }
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

/// The parent-side error for a reported (stage, errno). `spawn` / `exec`
/// (CRuby's `rb_exec_fail`) name a failed `chdir` by its directory;
/// `IO.popen` (`pipe_open`) names every failed call by the call.
fn stage_error(
    globals: &Globals,
    spec: &ExecSpec,
    stage: u8,
    errno: i32,
    popen: bool,
) -> MonorubyErr {
    let err = std::io::Error::from_raw_os_error(errno);
    match stage {
        STAGE_EXEC => MonorubyErr::errno_with_msg(&globals.store, &err, &spec.program_display),
        STAGE_CHDIR if !popen => {
            let dir = spec.chdir.as_ref().map(|(_, d)| d.as_str()).unwrap_or("");
            MonorubyErr::errno_with_msg(&globals.store, &err, dir)
        }
        _ => MonorubyErr::errno_with_msg(&globals.store, &err, stage_name(stage)),
    }
}

/// The `execve` argument vectors, built in the parent: the child may not
/// allocate (under `clone(CLONE_VM)` it would be using the parent's
/// heap).
struct ExecPtrs {
    argv: Vec<*const libc::c_char>,
    envp: Vec<*const libc::c_char>,
    /// `/bin/sh <candidate> <args...>` for a script without a shebang;
    /// the child writes the candidate into slot 1.
    sh_argv: Vec<*const libc::c_char>,
}

impl ExecPtrs {
    fn new(spec: &ExecSpec) -> Self {
        let nul = std::iter::once(std::ptr::null());
        let argv: Vec<_> = spec
            .argv
            .iter()
            .map(|a| a.as_ptr())
            .chain(nul.clone())
            .collect();
        let envp = spec
            .envp
            .iter()
            .map(|e| e.as_ptr())
            .chain(nul.clone())
            .collect();
        let sh_argv = [c"/bin/sh".as_ptr(), std::ptr::null()]
            .into_iter()
            .chain(argv.iter().skip(1).copied())
            .collect();
        ExecPtrs {
            argv,
            envp,
            sh_argv,
        }
    }
}

/// The child's side of [`fork_exec`]: run [`child_exec`] and, when it
/// returns (the exec failed), report `(stage, errno)` on `wfd` and leave.
///
/// # Safety
/// Only in the child, before anything else.
unsafe fn child_main(
    spec: &ExecSpec,
    fds: &mut ChildFds,
    ptrs: &mut ExecPtrs,
    rfd: i32,
    wfd: i32,
) -> ! {
    // SAFETY: the caller's contract; only async-signal-safe calls.
    unsafe {
        libc::close(rfd);
        let (stage, e) = child_exec(spec, fds, ptrs);
        let b = e.to_le_bytes();
        let buf = [stage, b[0], b[1], b[2], b[3]];
        let _ = libc::write(wfd, buf.as_ptr() as *const libc::c_void, buf.len());
        // `_exit` skips the profiling runtime's exit hook, which would
        // leave the child-side failure paths above invisible to
        // coverage (a no-op outside an instrumented build).
        crate::fork::flush_coverage_profile();
        libc::_exit(127)
    }
}

/// Start the child, answering its pid.
///
/// On Linux this is CRuby's `vfork`, as `clone(CLONE_VM | CLONE_VFORK)`
/// onto a stack of its own (the child never returns into the parent's
/// frames): the parent's page tables are not copied, which a `fork(2)` of
/// a large heap pays for on every spawn, and the calling thread waits
/// until the child has exec'd or exited. The child shares the parent's
/// memory and touches only `spec`, `fds` and `ptrs`; all signals stay
/// blocked across the clone so no handler of ours runs in it before
/// `child_exec` has reset them. An instrumented (coverage) build forks
/// instead: a child sharing the counters would write the parent's into
/// its own profile.
#[cfg(all(target_os = "linux", not(coverage)))]
fn start_child(
    spec: &ExecSpec,
    fds: &mut ChildFds,
    ptrs: &mut ExecPtrs,
    rfd: i32,
    wfd: i32,
) -> std::io::Result<i32> {
    struct Args<'a> {
        spec: &'a ExecSpec,
        fds: &'a mut ChildFds,
        ptrs: &'a mut ExecPtrs,
        rfd: i32,
        wfd: i32,
    }
    extern "C" fn entry(arg: *mut libc::c_void) -> libc::c_int {
        // SAFETY: `arg` is the `Args` below, alive until the clone
        // returns, which is after this child has exec'd or exited.
        unsafe {
            let a = &mut *(arg as *mut Args);
            child_main(a.spec, a.fds, a.ptrs, a.rfd, a.wfd)
        }
    }
    const STACK_SIZE: usize = 256 * 1024;
    let mut args = Args {
        spec,
        fds,
        ptrs,
        rfd,
        wfd,
    };
    // SAFETY: a fresh private mapping for the child's stack, unmapped once
    // the clone has returned; the mask is restored right after the clone.
    unsafe {
        let stack = libc::mmap(
            std::ptr::null_mut(),
            STACK_SIZE,
            libc::PROT_READ | libc::PROT_WRITE,
            libc::MAP_PRIVATE | libc::MAP_ANONYMOUS | libc::MAP_STACK,
            -1,
            0,
        );
        if stack == libc::MAP_FAILED {
            return Err(std::io::Error::last_os_error());
        }
        let mut all: libc::sigset_t = std::mem::zeroed();
        let mut old: libc::sigset_t = std::mem::zeroed();
        libc::sigfillset(&mut all);
        libc::pthread_sigmask(libc::SIG_SETMASK, &all, &mut old);
        let pid = libc::clone(
            entry,
            (stack as *mut u8).add(STACK_SIZE) as *mut libc::c_void,
            libc::CLONE_VM | libc::CLONE_VFORK | libc::SIGCHLD,
            &mut args as *mut Args as *mut libc::c_void,
        );
        let err = std::io::Error::last_os_error();
        libc::pthread_sigmask(libc::SIG_SETMASK, &old, std::ptr::null_mut());
        libc::munmap(stack, STACK_SIZE);
        if pid < 0 { Err(err) } else { Ok(pid) }
    }
}

/// Start the child with `fork(2)`, answering its pid. Quiesces the
/// process across the fork, as `Process._fork` does (`crate::fork`). This
/// child only `execve`s or `_exit`s, so it never takes these locks itself
/// — holding them keeps the invariant uniform across every `fork(2)` in
/// the tree, so anything added between the fork and the exec cannot
/// reintroduce the hazard.
#[cfg(not(all(target_os = "linux", not(coverage))))]
fn start_child(
    spec: &ExecSpec,
    fds: &mut ChildFds,
    ptrs: &mut ExecPtrs,
    rfd: i32,
    wfd: i32,
) -> std::io::Result<i32> {
    let guards = crate::fork::prepare(crate::fork::ForkKind::Exec);
    // SAFETY: every lock the child could need is held (`guards`), and
    // everything it touches was prepared pre-fork.
    let pid = unsafe { libc::fork() };
    if pid == 0 {
        // SAFETY: freshly forked, single-threaded.
        unsafe { child_main(spec, fds, ptrs, rfd, wfd) }
    }
    let err = std::io::Error::last_os_error();
    drop(guards);
    if pid < 0 { Err(err) } else { Ok(pid) }
}

/// fork + [`child_exec`], with exec failure reported through a CLOEXEC
/// pipe. Answers the child's pid and, when it could not exec, the
/// `(stage, errno)` it reported — the child is reaped then, so `$?` is
/// CRuby's 127.
fn fork_exec(
    vm: &mut Executor,
    globals: &mut Globals,
    spec: &ExecSpec,
    fds: &mut ChildFds,
) -> Result<(i32, Option<(u8, i32)>)> {
    // Hand our buffered stdout/stderr to the kernel first: the child
    // inherits a copy of the buffer along with the fds, so anything left
    // in it would be written twice (or, once the child execs, land after
    // output the child produced first). CRuby flushes before its forks
    // for the same reason.
    crate::rvalue::io::flush_std_streams();
    // CLOEXEC so a successful execve closes the write end and the
    // parent sees EOF.
    let (rfd, mut wfd) =
        pipe_cloexec().map_err(|err| MonorubyErr::errno_with_msg(&globals.store, &err, "pipe2"))?;
    // Keep the report pipe clear of every fd the redirects touch, or a
    // `dup2` onto its number would send the report somewhere else.
    let max_fd = fds.max_fd();
    if wfd <= max_fd {
        // SAFETY: duplicating our own fd above `max_fd`, close-on-exec.
        let moved = unsafe { libc::fcntl(wfd, libc::F_DUPFD_CLOEXEC, max_fd + 1) };
        if moved >= 0 {
            // SAFETY: the original is ours and replaced by `moved`.
            unsafe { libc::close(wfd) };
            wfd = moved;
        }
    }
    let mut ptrs = ExecPtrs::new(spec);
    let pid = match start_child(spec, fds, &mut ptrs, rfd, wfd) {
        Ok(pid) => pid,
        Err(err) => {
            // SAFETY: our own descriptors.
            unsafe {
                libc::close(rfd);
                libc::close(wfd);
            }
            return Err(MonorubyErr::errno_with_msg(&globals.store, &err, "fork"));
        }
    };
    // SAFETY: plain fd syscalls on fds owned here.
    unsafe {
        libc::close(wfd);
        if let Some(slave) = spec.pty_slave() {
            release_pty_slave(slave);
        }
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
    if n < 5 {
        return Ok((pid, None));
    }
    // Exec failed: reap the child so $? carries exit status 127.
    let stage = buf[0];
    let e = i32::from_le_bytes([buf[1], buf[2], buf[3], buf[4]]);
    let mut raw: i32 = 0;
    // SAFETY: waitpid on the child we just forked.
    let r = unsafe { libc::waitpid(pid, &mut raw, 0) };
    if r == pid {
        set_status(vm, globals, raw, pid)?;
    }
    Ok((pid, Some((stage, e))))
}

/// `Process.spawn`: fork + child_exec, reporting exec failure through a
/// CLOEXEC pipe. On failure the child is reaped (so `$?` is the CRuby
/// 127) and the `Errno::*` raised in the parent.
pub(crate) fn do_spawn(vm: &mut Executor, globals: &mut Globals, spec: &ExecSpec) -> Result<i64> {
    spawn_reporting_child(vm, globals, spec)?
}

/// [`do_spawn`], keeping apart the two ways it fails: the outer error
/// is the parent's own (a redirect file that cannot be opened, a bad
/// redirect, a failed fork), the inner one what the child reported back
/// — which `system` turns into `nil` rather than raising.
pub(crate) fn spawn_reporting_child(
    vm: &mut Executor,
    globals: &mut Globals,
    spec: &ExecSpec,
) -> Result<Result<i64>> {
    let mut fds = prepare_child_fds(vm, globals, spec, &[])?;
    Ok(match fork_exec(vm, globals, spec, &mut fds)? {
        (pid, None) => Ok(pid as i64),
        (_, Some((stage, e))) => Err(stage_error(globals, spec, stage, e, false)),
    })
}

/// `IO.popen`'s child: `spec` run with its stdin and/or stdout on fresh
/// pipes (CRuby's `pipe_open`). Answers the pid and this process's ends:
/// the one to read the child's stdout from, the one to write its stdin to.
pub(crate) fn do_popen(
    vm: &mut Executor,
    globals: &mut Globals,
    spec: &ExecSpec,
    readable: bool,
    writable: bool,
) -> Result<(
    i32,
    Option<std::os::fd::OwnedFd>,
    Option<std::os::fd::OwnedFd>,
)> {
    use std::os::fd::{FromRawFd, OwnedFd};
    let pipe = |globals: &Globals| {
        pipe_cloexec()
            .map(|(r, w)| {
                // SAFETY: fresh descriptors, owned from here on.
                unsafe { (OwnedFd::from_raw_fd(r), OwnedFd::from_raw_fd(w)) }
            })
            .map_err(|err| MonorubyErr::errno_with_msg(&globals.store, &err, &spec.program_display))
    };
    use std::os::fd::AsRawFd;
    let stdin_pipe = if writable { Some(pipe(globals)?) } else { None };
    let stdout_pipe = if readable { Some(pipe(globals)?) } else { None };
    let mut extra = vec![];
    if let Some((r, _)) = &stdin_pipe {
        extra.push((0, r.as_raw_fd()));
    }
    if let Some((_, w)) = &stdout_pipe {
        extra.push((1, w.as_raw_fd()));
    }
    let mut fds = prepare_child_fds(vm, globals, spec, &extra)?;
    match fork_exec(vm, globals, spec, &mut fds)? {
        (pid, None) => Ok((pid, stdout_pipe.map(|(r, _)| r), stdin_pipe.map(|(_, w)| w))),
        (_, Some((stage, e))) => Err(stage_error(globals, spec, stage, e, true)),
    }
}

/// `Process.exec`: apply the spec in-process and `execve`. Only returns
/// on failure (raising the `Errno::*`).
pub(crate) fn do_exec(vm: &mut Executor, globals: &mut Globals, spec: &ExecSpec) -> MonorubyErr {
    let mut fds = match prepare_child_fds(vm, globals, spec, &[]) {
        Ok(fds) => fds,
        Err(err) => return err,
    };
    let mut ptrs = ExecPtrs::new(spec);
    // SAFETY: exec replaces this process; on failure only fd/process
    // state that CRuby also clobbers has changed.
    let (stage, e) = unsafe { child_exec(spec, &mut fds, &mut ptrs) };
    stage_error(globals, spec, stage, e, false)
}

/// CRuby's `NUM2UINT` (what `NUM2MODET` is on Linux): a Float is
/// truncated (NaN / Infinity are a RangeError), and an Integer must fit
/// the combined `int` / `unsigned int` window, with CRuby's messages for
/// each way out of it.
fn num2uint(vm: &mut Executor, globals: &mut Globals, v: Value) -> Result<u32> {
    let i: i128 = if let Some(f) = v.try_float() {
        if f.is_nan() || f.is_infinite() {
            let s = if f.is_nan() {
                "NaN"
            } else if f > 0.0 {
                "Inf"
            } else {
                "-Inf"
            };
            return Err(MonorubyErr::rangeerr(format!(
                "float {s} out of range of integer"
            )));
        }
        f.trunc() as i128
    } else {
        let int = v.coerce_to_int(vm, globals)?;
        match int.unpack() {
            RV::Fixnum(i) => i as i128,
            RV::BigInt(b) => b
                .to_i128()
                .unwrap_or(if b.sign() == num::bigint::Sign::Minus {
                    i128::MIN
                } else {
                    i128::MAX
                }),
            _ => unreachable!(),
        }
    };
    if (-(1i128 << 31)..(1i128 << 32)).contains(&i) {
        Ok(i as u32)
    } else if (-(1i128 << 63)..(1i128 << 64)).contains(&i) {
        let side = if i > 0 { "big" } else { "small" };
        Err(MonorubyErr::rangeerr(format!(
            "integer {i} too {side} to convert to 'unsigned int'"
        )))
    } else if i < 0 {
        Err(MonorubyErr::rangeerr(
            "bignum out of range of unsigned long",
        ))
    } else {
        Err(MonorubyErr::rangeerr(
            "bignum too big to convert into 'unsigned long'",
        ))
    }
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
    fn spawn_child_side_failures() {
        // A redirect file is opened in the parent and a failure names its
        // path; a call that fails in the child comes back through the
        // status pipe and is named by the call (#1862).
        run_test_once(
            r#"
            [
              -> { Process.wait Process.spawn("true", err: 99) },
              -> { Process.wait Process.spawn("true", err: "/nonexistent-dir-xyzzy/x") },
              -> { Process.wait Process.spawn("true", in: "/nonexistent-dir-xyzzy/x") },
              -> { Process.wait Process.spawn("true", out: "/") },
              -> { Process.wait Process.spawn("true", pgroup: 999999) },
              -> { Process.wait Process.spawn("true", chdir: "/nonexistent-dir-xyzzy") },
              -> { Process.wait Process.spawn("true", 99 => :close) },
              -> { Process.wait Process.spawn("true", 99 => 99) },
              -> { IO.popen(["true"], err: 99) {} },
              -> { IO.popen(["true"], err: "/nonexistent-dir-xyzzy/x") {} },
              -> { system("true", err: "/nonexistent-dir-xyzzy/x") },
              -> { system("true", err: 99) },
            ].map do |f|
              r = f.call
              [:ok, r]
            rescue SystemCallError => e
              [e.class, e.message]
            end
            "#,
        );
    }

    #[test]
    fn spawn_redirects_checked_in_the_parent() {
        // CRuby's `check_exec_fds`, before anything forks.
        run_test_once(
            r#"
            [
              -> { Process.spawn("true", err: [:child, 99]) },
              -> { Process.spawn("true", err: [:child, :out]) },
              -> { Process.spawn("true", 3 => [:child, 4], 4 => [:child, 3]) },
              -> { Process.spawn("true", 2 => :close, err: File::NULL) },
              -> { Process.spawn("true", [:out, 1] => File::NULL) },
              -> { system("true", err: [:child, 99]) },
            ].map do |f|
              f.call
              :no_error
            rescue ArgumentError => e
              e.message
            end
            "#,
        );
    }

    #[test]
    fn spawn_redirect_order() {
        // The dup2s run before the `[:child, fd]`s whatever the option
        // order, a swap goes through a spare fd, a chain of
        // `[:child, fd]` resolves to its end, and a redirect path is
        // relative to the parent's directory, not `chdir:`'s.
        run_test_once(
            r##"
            require "tmpdir"
            Dir.mktmpdir do |d|
              res = []
              Process.wait Process.spawn("echo e >&2", err: [:child, :out], out: "#{d}/a")
              res << File.read("#{d}/a")
              r, w = IO.pipe
              Process.wait Process.spawn("echo o; echo e >&2", out: w, err: :out)
              Process.wait Process.spawn("echo o3 >&3; echo o4 >&4", 3 => w, 4 => 3)
              Process.wait Process.spawn("echo c >&5", 4 => w, 3 => [:child, 4], 5 => [:child, 3])
              w.close
              res << r.read
              Dir.chdir(d) { Process.wait Process.spawn("pwd", chdir: "/", out: "rel") }
              res << File.read("#{d}/rel")
              Process.wait Process.spawn("sh", "-c", "echo $FOO", unsetenv_others: true, out: "#{d}/env")
              res << File.read("#{d}/env")
            end
            "##,
        );
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
    fn exec_applies_child_setup_in_process() {
        // Run a full option set through `do_exec`'s child_exec in a
        // forked child whose exec FAILS: pgroup/umask/chdir/redirects all
        // apply in-process (recorded by coverage, verified via Dir.pwd),
        // then the ENOENT surfaces as an in-process raise. The child
        // exits normally so its profile is flushed.
        run_test_once(
            r#"
            r, w = IO.pipe
            pid = fork do
              r.close
              res = nil
              begin
                Process.exec(
                  {"EXEC_T" => "1"},
                  "definitely-no-such-cmd-xyzzy",
                  out: "/dev/null",
                  err: [:child, :out],
                  umask: 0,
                  pgroup: 0,
                  chdir: "/",
                )
              rescue Errno::ENOENT
                res = [Dir.pwd, File.umask, Process.getpgrp == Process.pid]
              end
              w.write(res.inspect)
              w.close
              exit
            end
            w.close
            res = r.read
            Process.wait(pid)
            res
            "#,
        );
    }

    #[test]
    fn spawn_redirect_parse_forms() {
        // Exercise the remaining redirect-value shapes; each spawns
        // `true` (cheap) or validates without forking.
        run_test_once(
            r##"
            require "tmpdir"
            res = []
            Dir.mktmpdir do |d|
              # [path, mode-string], [path, flags-Integer, perm]
              Process.wait Process.spawn("echo b", out: ["#{d}/r1", "a"])
              Process.wait Process.spawn("echo c", out: ["#{d}/r2", File::WRONLY | File::CREAT, 0o600])
              res << File.read("#{d}/r1")
              res << File.read("#{d}/r2")
              # :in from a file: bare-String value and mode-less Array
              # value (the latter defaults to read-only, CRuby-style)
              File.write("#{d}/in", "stdin-data")
              rr, ww = IO.pipe
              Process.wait Process.spawn("cat", in: "#{d}/in", out: ww)
              ww.close
              res << rr.read
              rr2, ww2 = IO.pipe
              Process.wait Process.spawn("cat", in: ["#{d}/in"], out: ww2)
              ww2.close
              res << rr2.read
              # IO value on :in + Integer fd value + symbol target
              File.open("#{d}/in") do |f|
                r2, w2 = IO.pipe
                Process.wait Process.spawn("cat", in: f, out: w2.fileno, err: :out)
                w2.close
                res << r2.read
              end
              # #to_io object as redirect value
              wrapper = Object.new
              File.open("#{d}/r3", "w") do |f|
                wrapper.define_singleton_method(:to_io) { f }
                Process.wait Process.spawn("echo via-to-io", out: wrapper)
              end
              res << File.read("#{d}/r3")
              # IO object as redirect KEY
              File.open("#{d}/r4", "w") do |f|
                Process.wait Process.spawn("echo io-key >&#{f.fileno}", f => f.fileno)
                # (fd => fd form is covered by ruby/spec)
              end
            end
            res
            "##,
        );
        run_test_error(r#"Process.spawn("true", out: :bogus_target)"#);
        run_test_error(r#"Process.spawn("true", out: Object.new)"#);
        run_test_error(r#"Process.spawn("true", [:out, :bogus] => "/dev/null")"#);
        run_test_error(r#"Process.spawn("true", out: [:child, :bogus])"#);
    }

    #[test]
    fn spawn_enoexec_sh_fallback_and_coercions() {
        // A +x script without a shebang: execve fails ENOEXEC and the
        // child retries through /bin/sh, like execvp.
        run_test_once(
            r##"
            require "tmpdir"
            Dir.mktmpdir do |d|
              path = "#{d}/script"
              File.write(path, "echo no-shebang
")
              File.chmod(0o755, path)
              r, w = IO.pipe
              Process.wait Process.spawn(path, out: w)
              w.close
              r.read
            end
            "##,
        );
        // env via #to_hash, command array via #to_ary (with extra argv).
        run_test_once(
            r#"
            env = Object.new
            def env.to_hash = {"VIA_TO_HASH" => "yes"}
            cmd = Object.new
            def cmd.to_ary = ["/bin/sh", "shname"]
            r, w = IO.pipe
            Process.wait Process.spawn(env, cmd, "-c", "echo $VIA_TO_HASH-$0", out: w)
            w.close
            r.read
            "#,
        );
    }

    #[test]
    fn spawn_close_others_and_env_paths() {
        run_test_once(
            r#"
            # close_others: true still keeps the redirected fds.
            r, w = IO.pipe
            Process.wait Process.spawn("echo kept", out: w, close_others: true)
            w.close
            r.read
            "#,
        );
        run_test_once(
            r#"
            # unsetenv_others with an env hash keeps only the given vars
            # (PATH-less exec still works via the absolute-path shell).
            r, w = IO.pipe
            Process.wait Process.spawn({"KEEP" => "yes"}, "echo [$KEEP][$HOME]", unsetenv_others: true, out: w)
            w.close
            r.read
            "#,
        );
        // env-key/value #to_str coercion.
        run_test_once(
            r#"
            k = Object.new
            def k.to_str = "COERCED"
            v = Object.new
            def v.to_str = "val"
            r, w = IO.pipe
            Process.wait Process.spawn({k => v}, "echo $COERCED", out: w)
            w.close
            r.read
            "#,
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

    #[test]
    fn spawn_umask_range() {
        run_test_once(
            r##"
            [Float::NAN, Float::INFINITY, 2**32, 2**64 - 1, 2**64, -2**63 - 1, -2**31 - 1].map do |m|
              begin
                Process.spawn("should_raise_before_and_not_run", umask: m)
              rescue RangeError => e
                [e.class, e.message]
              end
            end
            "##,
        );
    }
}
