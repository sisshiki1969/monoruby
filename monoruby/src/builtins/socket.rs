use super::*;
use std::os::fd::FromRawFd;

//
// TCP sockets: BasicSocket / IPSocket / TCPSocket / TCPServer / Socket.
//
// A socket is an ordinary IO object (`ObjTy::IO` wrapping the socket fd,
// classed as TCPSocket / TCPServer), so the entire IO surface —
// read/gets/write/print/puts/close/closed?/fileno/eof?, and crucially the
// green-thread blocking-IO emulation (mid-operation would-block parks the
// calling thread on the scheduler's fd poller) — applies to sockets
// unchanged via class inheritance. This module adds only the
// socket-specific surface: the constructors (socket/bind/listen/connect),
// `accept`, addresses, socket options, and `shutdown`.
//
// Blocking discipline (M:1 green threads):
// - the listener fd is permanently non-blocking; `accept` retries around
//   `EAGAIN`, parking on the fd poller (or a plain poll(2) when no other
//   green thread could run).
// - `connect` is issued non-blocking (`EINPROGRESS`), parks on POLLOUT,
//   then reads `SO_ERROR`; the fd is restored to blocking mode afterwards
//   so the ordinary IO paths see a plain blocking stream.
//

pub(super) fn init(globals: &mut Globals) {
    let io_class = globals
        .store
        .get_constant_noautoload(OBJECT_CLASS, IdentId::get_id("IO"))
        .unwrap()
        .as_class();
    let standard_error = globals
        .store
        .get_constant_noautoload(OBJECT_CLASS, IdentId::get_id("StandardError"))
        .unwrap()
        .as_class();

    let basic = globals.define_class("BasicSocket", io_class, OBJECT_CLASS);
    let basic_id = basic.id();
    let ipsocket = globals.define_class("IPSocket", basic, OBJECT_CLASS);
    let ip_id = ipsocket.id();
    let tcpsocket = globals.define_class("TCPSocket", ipsocket, OBJECT_CLASS);
    let tcps_id = tcpsocket.id();
    let tcpserver = globals.define_class("TCPServer", tcpsocket, OBJECT_CLASS);
    let tsrv_id = tcpserver.id();
    let socket_class = globals.define_class("Socket", basic, OBJECT_CLASS);
    let sock_id = socket_class.id();
    globals.define_class("SocketError", standard_error, OBJECT_CLASS);

    // Socket::Constants, and the same constants directly on Socket
    // (CRuby's Socket includes Socket::Constants).
    let consts_id = globals
        .define_class("Constants", None, sock_id)
        .id();
    for (name, v) in SOCKET_CONSTANTS {
        let id = IdentId::get_id(name);
        globals.set_constant(sock_id, id, Value::integer(*v));
        globals.set_constant(consts_id, id, Value::integer(*v));
    }

    globals.define_builtin_class_func_with(tsrv_id, "new", tcp_server_new, 1, 2, false);
    globals.define_builtin_class_func_with(tsrv_id, "open", tcp_server_new, 1, 2, false);
    globals.define_builtin_func(tsrv_id, "accept", tcp_server_accept, 0);
    globals.define_builtin_func(tsrv_id, "listen", socket_listen, 1);

    // `connect_timeout:` / `open_timeout:` (net/http and Socket.tcp pass
    // them) are accepted; nil/absent means wait indefinitely.
    globals.define_builtin_class_func_with_kw(
        tcps_id,
        "new",
        tcp_socket_new,
        2,
        4,
        false,
        &["connect_timeout", "open_timeout"],
        false,
    );
    globals.define_builtin_class_func_with_kw(
        tcps_id,
        "open",
        tcp_socket_new,
        2,
        4,
        false,
        &["connect_timeout", "open_timeout"],
        false,
    );

    // Socket.new(domain, type, protocol = 0): a real socket(2). Without
    // this, `Socket.new(Socket::AF_INET, ...)` fell through to the
    // inherited `IO.new(2, ...)` — which took *ownership of stderr*
    // (AF_INET == 2 read as an fd) and closed it later, cascading into
    // fd-2 reuse chaos across a whole mspec process.
    globals.define_builtin_class_func_with(sock_id, "new", socket_new, 2, 3, false);
    globals.define_builtin_class_func_with(sock_id, "open", socket_new, 2, 3, false);

    globals.define_builtin_class_func(ip_id, "getaddress", ip_getaddress, 1);
    globals.define_builtin_func_with(ip_id, "addr", socket_addr, 0, 1, false);
    globals.define_builtin_func_with(ip_id, "peeraddr", socket_peeraddr, 0, 1, false);

    globals.define_builtin_func(basic_id, "setsockopt", setsockopt, 3);
    globals.define_builtin_func(basic_id, "getsockopt", getsockopt, 2);
    globals.define_builtin_func_with(basic_id, "shutdown", shutdown_, 0, 1, false);
}

/// The socket constants exposed as `Socket::X` / `Socket::Constants::X`.
/// Values come from libc so they are correct per target OS.
const SOCKET_CONSTANTS: &[(&str, i64)] = &[
    ("AF_UNSPEC", libc::AF_UNSPEC as i64),
    ("AF_INET", libc::AF_INET as i64),
    ("AF_INET6", libc::AF_INET6 as i64),
    ("AF_UNIX", libc::AF_UNIX as i64),
    ("PF_UNSPEC", libc::PF_UNSPEC as i64),
    ("PF_INET", libc::PF_INET as i64),
    ("PF_INET6", libc::PF_INET6 as i64),
    ("PF_UNIX", libc::PF_UNIX as i64),
    ("SOCK_STREAM", libc::SOCK_STREAM as i64),
    ("SOCK_DGRAM", libc::SOCK_DGRAM as i64),
    ("SOCK_RAW", libc::SOCK_RAW as i64),
    ("SOCK_SEQPACKET", libc::SOCK_SEQPACKET as i64),
    ("SOL_SOCKET", libc::SOL_SOCKET as i64),
    ("IPPROTO_IP", libc::IPPROTO_IP as i64),
    ("IPPROTO_TCP", libc::IPPROTO_TCP as i64),
    ("IPPROTO_UDP", libc::IPPROTO_UDP as i64),
    ("IPPROTO_IPV6", libc::IPPROTO_IPV6 as i64),
    ("TCP_NODELAY", libc::TCP_NODELAY as i64),
    ("SO_REUSEADDR", libc::SO_REUSEADDR as i64),
    ("SO_REUSEPORT", libc::SO_REUSEPORT as i64),
    ("SO_KEEPALIVE", libc::SO_KEEPALIVE as i64),
    ("SO_ERROR", libc::SO_ERROR as i64),
    ("SO_LINGER", libc::SO_LINGER as i64),
    ("SO_RCVBUF", libc::SO_RCVBUF as i64),
    ("SO_SNDBUF", libc::SO_SNDBUF as i64),
    ("SO_TYPE", libc::SO_TYPE as i64),
    ("SO_BROADCAST", libc::SO_BROADCAST as i64),
    ("SO_OOBINLINE", libc::SO_OOBINLINE as i64),
    ("SHUT_RD", libc::SHUT_RD as i64),
    ("SHUT_WR", libc::SHUT_WR as i64),
    ("SHUT_RDWR", libc::SHUT_RDWR as i64),
    ("MSG_PEEK", libc::MSG_PEEK as i64),
    ("MSG_OOB", libc::MSG_OOB as i64),
    ("MSG_WAITALL", libc::MSG_WAITALL as i64),
    ("INADDR_ANY", 0),
    ("INADDR_LOOPBACK", 0x7f00_0001),
    ("INADDR_BROADCAST", 0xffff_ffff),
    ("IPV6_V6ONLY", libc::IPV6_V6ONLY as i64),
    ("SOMAXCONN", libc::SOMAXCONN as i64),
];

/// `SocketError` with the given message.
fn socket_error(store: &Store, msg: impl Into<String>) -> MonorubyErr {
    let cid = store
        .get_constant_noautoload(OBJECT_CLASS, IdentId::get_id("SocketError"))
        .and_then(|v| v.is_class_or_module())
        .map(|m| m.id());
    match cid {
        Some(cid) => MonorubyErr::new(MonorubyErrKind::Other(cid), msg.into()),
        None => MonorubyErr::runtimeerr(msg.into()),
    }
}

/// The appropriate `Errno::E*` for the current `errno`.
fn last_errno_err(store: &Store, ctx: &str) -> MonorubyErr {
    let err = std::io::Error::last_os_error();
    let msg = format!("{} - {}", err, ctx);
    MonorubyErr::from_io_err(store, &err, msg)
}

fn errno_err(store: &Store, errno: i32, ctx: &str) -> MonorubyErr {
    let err = std::io::Error::from_raw_os_error(errno);
    let msg = format!("{} - {}", err, ctx);
    MonorubyErr::from_io_err(store, &err, msg)
}

/// host argument: nil → None, String → Some. Anything else is CRuby's
/// implicit-conversion TypeError.
fn value_to_host(globals: &Globals, v: Value) -> Result<Option<String>> {
    if v.is_nil() {
        return Ok(None);
    }
    match v.is_str() {
        Some(s) if s.is_empty() => Ok(None),
        Some(s) => Ok(Some(s.to_string())),
        None => Err(MonorubyErr::typeerr(format!(
            "no implicit conversion of {} into String",
            globals.store.get_class_name(v.class())
        ))),
    }
}

/// port argument: Integer, or a numeric String ("0", "8080").
fn value_to_port(globals: &Globals, v: Value) -> Result<u16> {
    if let Some(i) = v.try_fixnum() {
        if (0..=65535).contains(&i) {
            return Ok(i as u16);
        }
        return Err(socket_error(&globals.store, "getaddrinfo: Servname not supported"));
    }
    if let Some(s) = v.is_str()
        && let Ok(i) = s.trim().parse::<u16>()
    {
        return Ok(i);
    }
    Err(socket_error(
        &globals.store,
        "getaddrinfo: Servname not supported for ai_socktype",
    ))
}

/// Resolve `host` to an IPv4 address (network byte order). `for_bind`
/// selects the nil default: INADDR_ANY for servers, loopback for clients.
fn resolve_ipv4(store: &Store, host: Option<&str>, for_bind: bool) -> Result<u32> {
    let host = match host {
        None => {
            return Ok(if for_bind {
                u32::to_be(libc::INADDR_ANY)
            } else {
                u32::to_be(libc::INADDR_LOOPBACK)
            });
        }
        Some(h) => h,
    };
    if let Ok(ip) = host.parse::<std::net::Ipv4Addr>() {
        return Ok(u32::from_ne_bytes(ip.octets()));
    }
    // Name lookup via getaddrinfo(3), IPv4/stream only. Local names
    // (localhost, /etc/hosts entries) resolve without network traffic;
    // anything slower is accepted as a blocking call, like CRuby without
    // the resolv replacement.
    let c_host = std::ffi::CString::new(host)
        .map_err(|_| socket_error(store, "getaddrinfo: Name or service not known"))?;
    // SAFETY: hints is a plain zeroed struct we fill; res is an out
    // pointer freed with freeaddrinfo on success.
    unsafe {
        let mut hints: libc::addrinfo = std::mem::zeroed();
        hints.ai_family = libc::AF_INET;
        hints.ai_socktype = libc::SOCK_STREAM;
        let mut res: *mut libc::addrinfo = std::ptr::null_mut();
        let rc = libc::getaddrinfo(c_host.as_ptr(), std::ptr::null(), &hints, &mut res);
        if rc != 0 {
            let msg = std::ffi::CStr::from_ptr(libc::gai_strerror(rc)).to_string_lossy();
            return Err(socket_error(store, format!("getaddrinfo: {msg}")));
        }
        let mut cur = res;
        while !cur.is_null() {
            let ai = &*cur;
            if ai.ai_family == libc::AF_INET && !ai.ai_addr.is_null() {
                let sin = &*(ai.ai_addr as *const libc::sockaddr_in);
                let addr = sin.sin_addr.s_addr;
                libc::freeaddrinfo(res);
                return Ok(addr);
            }
            cur = ai.ai_next;
        }
        libc::freeaddrinfo(res);
        Err(socket_error(store, "getaddrinfo: Name or service not known"))
    }
}

fn sockaddr_in(addr_be: u32, port: u16) -> libc::sockaddr_in {
    // SAFETY: sockaddr_in is plain-old-data; zeroing is a valid initial state.
    let mut sin: libc::sockaddr_in = unsafe { std::mem::zeroed() };
    sin.sin_family = libc::AF_INET as libc::sa_family_t;
    sin.sin_port = port.to_be();
    sin.sin_addr = libc::in_addr { s_addr: addr_be };
    sin
}

/// A new non-blocking, close-on-exec TCP socket fd.
fn new_tcp_fd(store: &Store) -> Result<i32> {
    // SAFETY: plain socket(2).
    let fd = unsafe {
        libc::socket(
            libc::AF_INET,
            libc::SOCK_STREAM | libc::SOCK_CLOEXEC | libc::SOCK_NONBLOCK,
            0,
        )
    };
    if fd < 0 {
        return Err(last_errno_err(store, "socket(2)"));
    }
    Ok(fd)
}

/// Clear O_NONBLOCK: connected/accepted stream sockets are handed to the
/// generic IO layer in blocking mode (the blocking-IO emulation flips the
/// flag per operation as needed).
fn set_blocking(fd: i32) {
    // SAFETY: fcntl on an fd we own; best-effort.
    unsafe {
        let flags = libc::fcntl(fd, libc::F_GETFL);
        if flags >= 0 {
            libc::fcntl(fd, libc::F_SETFL, flags & !libc::O_NONBLOCK);
        }
    }
}

/// Park until `fd` reports `events`: on the scheduler's fd poller when
/// another green thread could run, else a plain signal-interruptible
/// poll(2).
fn park_on_fd(
    vm: &mut Executor,
    globals: &mut Globals,
    fd: i32,
    events: i16,
    deadline: Option<std::time::Instant>,
) -> Result<()> {
    if crate::scheduler::has_other_live_threads() {
        crate::scheduler::wait_fd(vm, globals, fd, events, deadline)
    } else if deadline.is_some() {
        // Deadline-limited single-thread wait: poll with a timeout.
        loop {
            let remain = deadline
                .unwrap()
                .saturating_duration_since(std::time::Instant::now());
            if remain.is_zero() {
                return Ok(());
            }
            let mut pfd = libc::pollfd {
                fd,
                events,
                revents: 0,
            };
            // SAFETY: one pollfd, length 1.
            let ret = unsafe { libc::poll(&mut pfd, 1, remain.as_millis().min(i32::MAX as u128) as i32) };
            if ret >= 0 {
                return Ok(());
            }
            let e = std::io::Error::last_os_error();
            if e.kind() != std::io::ErrorKind::Interrupted {
                return Err(MonorubyErr::from_io_err(&globals.store, &e, "poll(2)".into()));
            }
            if crate::executor::execute_gc(vm, globals).is_none() {
                return Err(vm.take_error());
            }
        }
    } else {
        crate::builtins::io::wait_fd_single(vm, globals, fd, events)
    }
}

/// The `TCPSocket` class (accepted connections are TCPSocket even when
/// accepted on a TCPServer subclass).
fn tcpsocket_class(store: &Store) -> ClassId {
    store
        .get_constant_noautoload(OBJECT_CLASS, IdentId::get_id("TCPSocket"))
        .unwrap()
        .as_class()
        .id()
}

///
/// ### TCPServer.new
///
/// - new([host,] port) -> TCPServer
///
/// Binds an IPv4 listener. The listener fd is left permanently
/// non-blocking for `accept`'s park-and-retry loop.
#[monoruby_builtin]
fn tcp_server_new(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let (host, port) = match lfp.try_arg(1) {
        Some(p) => (
            value_to_host(globals, lfp.arg(0))?,
            value_to_port(globals, p)?,
        ),
        None => (None, value_to_port(globals, lfp.arg(0))?),
    };
    let addr = resolve_ipv4(&globals.store, host.as_deref(), true)?;
    let fd = new_tcp_fd(&globals.store)?;
    let sin = sockaddr_in(addr, port);
    // SAFETY: fd is a fresh socket we own; sin is a valid sockaddr_in.
    unsafe {
        let one: i32 = 1;
        libc::setsockopt(
            fd,
            libc::SOL_SOCKET,
            libc::SO_REUSEADDR,
            &one as *const i32 as *const libc::c_void,
            std::mem::size_of::<i32>() as libc::socklen_t,
        );
        if libc::bind(
            fd,
            &sin as *const libc::sockaddr_in as *const libc::sockaddr,
            std::mem::size_of::<libc::sockaddr_in>() as libc::socklen_t,
        ) < 0
        {
            let err = last_errno_err(&globals.store, &format!("bind(2) for {:?} port {}",
                host.as_deref().unwrap_or("0.0.0.0"), port));
            libc::close(fd);
            return Err(err);
        }
        if libc::listen(fd, libc::SOMAXCONN) < 0 {
            let err = last_errno_err(&globals.store, "listen(2)");
            libc::close(fd);
            return Err(err);
        }
    }
    let class_id = lfp.self_val().as_class().id();
    // SAFETY: fd is a fresh, owned socket fd.
    let file = unsafe { std::fs::File::from_raw_fd(fd) };
    let res = Value::new_socket(file, "TCPServer".into(), class_id);
    if let Some(bh) = lfp.block() {
        // `TCPServer.open(...) { |server| ... }` closes at block exit,
        // like IO.open.
        let r = vm.invoke_block_once(globals, bh, &[res]);
        return super::file::block_close(vm, globals, res, r);
    }
    Ok(res)
}

///
/// ### TCPServer#accept
///
/// - accept -> TCPSocket
///
/// Parks the calling green thread until a connection arrives. Raises
/// IOError when the server is closed (also from another thread while
/// parked — the poller wakes on the dead fd and the closed check re-runs).
#[monoruby_builtin]
fn tcp_server_accept(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_ = lfp.self_val();
    let mut parked = false;
    loop {
        if self_.as_io_inner().is_closed() {
            // CRuby: "closed stream" for an accept on an already-closed
            // server; "stream closed in another thread" when the close
            // happened while this accept was parked.
            return Err(MonorubyErr::ioerr(if parked {
                "stream closed in another thread"
            } else {
                "closed stream"
            }));
        }
        let fd = self_.as_io_inner().fileno()?;
        // SAFETY: accept4 on our listener fd; out params unused (NULL).
        let conn = unsafe {
            libc::accept4(
                fd,
                std::ptr::null_mut(),
                std::ptr::null_mut(),
                libc::SOCK_CLOEXEC,
            )
        };
        if conn >= 0 {
            // SAFETY: conn is a fresh, owned fd from accept4.
            let file = unsafe { std::fs::File::from_raw_fd(conn) };
            return Ok(Value::new_socket(
                file,
                "TCPSocket".into(),
                tcpsocket_class(&globals.store),
            ));
        }
        let errno = std::io::Error::last_os_error()
            .raw_os_error()
            .unwrap_or(libc::EIO);
        match errno {
            libc::EAGAIN | libc::EINTR | libc::ECONNABORTED => {
                // Reach a poll point first (signal / preemption /
                // Thread#kill), then park until the fd is readable.
                if crate::executor::execute_gc(vm, globals).is_none() {
                    return Err(vm.take_error());
                }
                if errno == libc::EAGAIN {
                    parked = true;
                    park_on_fd(vm, globals, fd, libc::POLLIN, None)?;
                }
            }
            _ => return Err(errno_err(&globals.store, errno, "accept(2)")),
        }
    }
}

///
/// ### TCPServer#listen
///
/// - listen(backlog) -> 0
///
#[monoruby_builtin]
fn socket_listen(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let backlog = lfp.arg(0).coerce_to_i64(&globals.store)? as i32;
    let fd = lfp.self_val().as_io_inner().fileno()?;
    // SAFETY: listen(2) on our fd.
    if unsafe { libc::listen(fd, backlog) } < 0 {
        return Err(last_errno_err(&globals.store, "listen(2)"));
    }
    Ok(Value::integer(0))
}

///
/// ### TCPSocket.new
///
/// - new(host, port, local_host = nil, local_port = nil, connect_timeout: nil, open_timeout: nil) -> TCPSocket
///
/// Non-blocking connect: `EINPROGRESS`, park on POLLOUT (bounded by
/// `connect_timeout` / `open_timeout`), then check `SO_ERROR`. The fd is
/// restored to blocking mode before it is handed to the IO layer.
#[monoruby_builtin]
fn tcp_socket_new(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let host = value_to_host(globals, lfp.arg(0))?;
    let port = value_to_port(globals, lfp.arg(1))?;
    let addr = resolve_ipv4(&globals.store, host.as_deref(), false)?;
    // kw slots: arg(4) = connect_timeout, arg(5) = open_timeout. CRuby
    // treats open_timeout as an overall bound (resolution + connect);
    // resolution is synchronous here, so both bound the connect wait.
    let mut timeout = None;
    for slot in [4, 5] {
        if let Some(v) = lfp.try_arg(slot)
            && !v.is_nil()
        {
            let secs = v.coerce_to_f64(vm, globals)?;
            let d = std::time::Duration::from_secs_f64(secs.max(0.0));
            timeout = Some(timeout.map_or(d, |t: std::time::Duration| t.min(d)));
        }
    }
    let fd = new_tcp_fd(&globals.store)?;
    // Optional local bind (local_host, local_port).
    if let Some(lh) = lfp.try_arg(2)
        && (!lh.is_nil() || lfp.try_arg(3).is_some_and(|v| !v.is_nil()))
    {
        let lhost = value_to_host(globals, lh)?;
        let lport = match lfp.try_arg(3) {
            Some(v) if !v.is_nil() => value_to_port(globals, v)?,
            _ => 0,
        };
        let laddr = resolve_ipv4(&globals.store, lhost.as_deref(), true)?;
        let lsin = sockaddr_in(laddr, lport);
        // SAFETY: bind(2) on our fresh fd.
        if unsafe {
            libc::bind(
                fd,
                &lsin as *const libc::sockaddr_in as *const libc::sockaddr,
                std::mem::size_of::<libc::sockaddr_in>() as libc::socklen_t,
            )
        } < 0
        {
            let err = last_errno_err(&globals.store, "bind(2)");
            // SAFETY: closing the fd we just opened.
            unsafe { libc::close(fd) };
            return Err(err);
        }
    }
    let sin = sockaddr_in(addr, port);
    // SAFETY: connect(2) on our fresh non-blocking fd.
    let rc = unsafe {
        libc::connect(
            fd,
            &sin as *const libc::sockaddr_in as *const libc::sockaddr,
            std::mem::size_of::<libc::sockaddr_in>() as libc::socklen_t,
        )
    };
    if rc < 0 {
        let errno = std::io::Error::last_os_error()
            .raw_os_error()
            .unwrap_or(libc::EIO);
        if errno != libc::EINPROGRESS && errno != libc::EINTR {
            // SAFETY: closing the fd we just opened.
            unsafe { libc::close(fd) };
            return Err(errno_err(&globals.store, errno, &connect_ctx(host.as_deref(), port)));
        }
        let deadline = timeout.map(|t| std::time::Instant::now() + t);
        loop {
            if let Err(e) = park_on_fd(vm, globals, fd, libc::POLLOUT, deadline) {
                // SAFETY: closing the fd we own.
                unsafe { libc::close(fd) };
                return Err(e);
            }
            // Connection settled (or deadline passed): read SO_ERROR.
            let mut soerr: i32 = 0;
            let mut len = std::mem::size_of::<i32>() as libc::socklen_t;
            // SAFETY: getsockopt out-params sized to an i32.
            unsafe {
                libc::getsockopt(
                    fd,
                    libc::SOL_SOCKET,
                    libc::SO_ERROR,
                    &mut soerr as *mut i32 as *mut libc::c_void,
                    &mut len,
                );
            }
            if soerr == 0 {
                // Either connected, or still in progress after a spurious
                // wake. Distinguish with a zero-timeout POLLOUT check.
                let mut pfd = libc::pollfd {
                    fd,
                    events: libc::POLLOUT,
                    revents: 0,
                };
                // SAFETY: one pollfd, length 1, timeout 0.
                let ready = unsafe { libc::poll(&mut pfd, 1, 0) };
                if ready > 0 {
                    break;
                }
                if let Some(dl) = deadline
                    && std::time::Instant::now() >= dl
                {
                    // SAFETY: closing the fd we own.
                    unsafe { libc::close(fd) };
                    return Err(errno_err(
                        &globals.store,
                        libc::ETIMEDOUT,
                        &connect_ctx(host.as_deref(), port),
                    ));
                }
                continue;
            }
            // SAFETY: closing the fd we own.
            unsafe { libc::close(fd) };
            return Err(errno_err(&globals.store, soerr, &connect_ctx(host.as_deref(), port)));
        }
    }
    set_blocking(fd);
    let class_id = lfp.self_val().as_class().id();
    // SAFETY: fd is a fresh, owned socket fd.
    let file = unsafe { std::fs::File::from_raw_fd(fd) };
    let res = Value::new_socket(file, "TCPSocket".into(), class_id);
    if let Some(bh) = lfp.block() {
        // `TCPSocket.open(...) { |sock| ... }` closes at block exit, like
        // IO.open — the net-http spec fixture stops its server exactly
        // this way (connect, write "CLOSE", implicit close).
        let r = vm.invoke_block_once(globals, bh, &[res]);
        return super::file::block_close(vm, globals, res, r);
    }
    Ok(res)
}

///
/// ### Socket.new
///
/// - new(domain, type, protocol = 0) -> Socket
///
/// Accepts Integer constants or their Symbol/String shorthand
/// (`:INET` / `"AF_INET"`, `:STREAM` / `"SOCK_STREAM"`).
#[monoruby_builtin]
fn socket_new(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let domain = family_const(&globals.store, lfp.arg(0))?;
    let stype = type_const(&globals.store, lfp.arg(1))?;
    let proto = match lfp.try_arg(2) {
        Some(v) if !v.is_nil() => v.coerce_to_i64(&globals.store)? as i32,
        _ => 0,
    };
    // SAFETY: plain socket(2).
    let fd = unsafe { libc::socket(domain, stype | libc::SOCK_CLOEXEC, proto) };
    if fd < 0 {
        return Err(last_errno_err(&globals.store, "socket(2)"));
    }
    let class_id = lfp.self_val().as_class().id();
    // SAFETY: fd is a fresh, owned socket fd.
    let file = unsafe { std::fs::File::from_raw_fd(fd) };
    Ok(Value::new_socket(file, "Socket".into(), class_id))
}

/// Address-family argument: Integer, or `:INET` / `"AF_INET"` style.
fn family_const(store: &Store, v: Value) -> Result<i32> {
    named_const(store, v, "AF_")
}

/// Socket-type argument: Integer, or `:STREAM` / `"SOCK_STREAM"` style.
fn type_const(store: &Store, v: Value) -> Result<i32> {
    named_const(store, v, "SOCK_")
}

fn named_const(store: &Store, v: Value, prefix: &str) -> Result<i32> {
    if let Some(i) = v.try_fixnum() {
        return Ok(i as i32);
    }
    let name = if let Some(s) = v.is_str() {
        s.to_string()
    } else if let Some(id) = v.try_symbol() {
        id.get_name()
    } else {
        return Err(MonorubyErr::typeerr(
            "socket domain/type should be an Integer, String or Symbol".to_string(),
        ));
    };
    let upper = name.to_uppercase();
    for cand in [upper.clone(), format!("{prefix}{upper}")] {
        if let Some((_, val)) = SOCKET_CONSTANTS.iter().find(|(n, _)| *n == cand) {
            return Ok(*val as i32);
        }
    }
    Err(socket_error(store, format!("unknown socket domain/type: {name}")))
}

fn connect_ctx(host: Option<&str>, port: u16) -> String {
    format!(
        "connect(2) for \"{}\" port {}",
        host.unwrap_or("127.0.0.1"),
        port
    )
}

///
/// ### IPSocket.getaddress
///
/// - getaddress(host) -> String
///
#[monoruby_builtin]
fn ip_getaddress(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let host = value_to_host(globals, lfp.arg(0))?;
    let addr = resolve_ipv4(&globals.store, host.as_deref(), false)?;
    Ok(Value::string(
        std::net::Ipv4Addr::from(u32::from_be(addr)).to_string(),
    ))
}

/// `["AF_INET", port, numeric_ip, numeric_ip]` for getsockname /
/// getpeername (numeric form — monoruby always behaves as
/// `do_not_reverse_lookup = true`).
fn addr_common(store: &Store, io: Value, peer: bool) -> Result<Value> {
    let fd = io.as_io_inner().fileno()?;
    // SAFETY: out-params sized to a sockaddr_in.
    let (sin, rc) = unsafe {
        let mut sin: libc::sockaddr_in = std::mem::zeroed();
        let mut len = std::mem::size_of::<libc::sockaddr_in>() as libc::socklen_t;
        let p = &mut sin as *mut libc::sockaddr_in as *mut libc::sockaddr;
        let rc = if peer {
            libc::getpeername(fd, p, &mut len)
        } else {
            libc::getsockname(fd, p, &mut len)
        };
        (sin, rc)
    };
    if rc < 0 {
        return Err(last_errno_err(
            store,
            if peer { "getpeername(2)" } else { "getsockname(2)" },
        ));
    }
    let ip = std::net::Ipv4Addr::from(u32::from_be(sin.sin_addr.s_addr)).to_string();
    let port = u16::from_be(sin.sin_port) as i64;
    Ok(Value::array_from_vec(vec![
        Value::string("AF_INET".to_string()),
        Value::integer(port),
        Value::string(ip.clone()),
        Value::string(ip),
    ]))
}

///
/// ### IPSocket#addr
///
#[monoruby_builtin]
fn socket_addr(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    addr_common(&globals.store, lfp.self_val(), false)
}

///
/// ### IPSocket#peeraddr
///
#[monoruby_builtin]
fn socket_peeraddr(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    addr_common(&globals.store, lfp.self_val(), true)
}

/// level / optname arguments: Integer, or Symbol/String constant name with
/// the usual CRuby prefix shorthand (`:REUSEADDR` == `SO_REUSEADDR`,
/// `:TCP` == `IPPROTO_TCP`, ...).
fn opt_const(store: &Store, v: Value) -> Result<i32> {
    if let Some(i) = v.try_fixnum() {
        return Ok(i as i32);
    }
    let name = if let Some(s) = v.is_str() {
        s.to_string()
    } else if let Some(id) = v.try_symbol() {
        id.get_name()
    } else {
        return Err(MonorubyErr::typeerr(
            "socket option should be an Integer, String or Symbol".to_string(),
        ));
    };
    let upper = name.to_uppercase();
    for cand in [
        upper.clone(),
        format!("SO_{upper}"),
        format!("SOL_{upper}"),
        format!("IPPROTO_{upper}"),
        format!("TCP_{upper}"),
    ] {
        if let Some((_, val)) = SOCKET_CONSTANTS.iter().find(|(n, _)| *n == cand) {
            return Ok(*val as i32);
        }
    }
    Err(socket_error(store, format!("unknown socket option: {name}")))
}

///
/// ### BasicSocket#setsockopt
///
/// - setsockopt(level, optname, optval) -> 0
///
#[monoruby_builtin]
fn setsockopt(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let level = opt_const(&globals.store, lfp.arg(0))?;
    let optname = opt_const(&globals.store, lfp.arg(1))?;
    let fd = lfp.self_val().as_io_inner().fileno()?;
    let v = lfp.arg(2);
    let bytes: Vec<u8> = if let Some(i) = v.try_fixnum() {
        (i as i32).to_ne_bytes().to_vec()
    } else if let Some(s) = v.is_rstring() {
        // Raw struct form (e.g. a packed linger).
        s.to_vec()
    } else {
        // true / false (and anything else truthy) as a boolean int option.
        (if v.as_bool() { 1i32 } else { 0i32 })
            .to_ne_bytes()
            .to_vec()
    };
    // SAFETY: setsockopt with a length-matched byte buffer.
    let rc = unsafe {
        libc::setsockopt(
            fd,
            level,
            optname,
            bytes.as_ptr() as *const libc::c_void,
            bytes.len() as libc::socklen_t,
        )
    };
    if rc < 0 {
        return Err(last_errno_err(&globals.store, "setsockopt(2)"));
    }
    Ok(Value::integer(0))
}

///
/// ### BasicSocket#getsockopt
///
/// - getsockopt(level, optname) -> Integer
///
/// Returns the option's raw int value (CRuby returns a Socket::Option;
/// the common `.int` / `.bool` consumers can use the Integer directly).
#[monoruby_builtin]
fn getsockopt(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let level = opt_const(&globals.store, lfp.arg(0))?;
    let optname = opt_const(&globals.store, lfp.arg(1))?;
    let fd = lfp.self_val().as_io_inner().fileno()?;
    let mut val: i32 = 0;
    let mut len = std::mem::size_of::<i32>() as libc::socklen_t;
    // SAFETY: out-params sized to an i32.
    let rc = unsafe {
        libc::getsockopt(
            fd,
            level,
            optname,
            &mut val as *mut i32 as *mut libc::c_void,
            &mut len,
        )
    };
    if rc < 0 {
        return Err(last_errno_err(&globals.store, "getsockopt(2)"));
    }
    Ok(Value::integer(val as i64))
}

///
/// ### BasicSocket#shutdown
///
/// - shutdown(how = Socket::SHUT_RDWR) -> 0
///
#[monoruby_builtin]
fn shutdown_(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let how = match lfp.try_arg(0) {
        None => libc::SHUT_RDWR,
        Some(v) => {
            if let Some(i) = v.try_fixnum() {
                i as i32
            } else {
                let name = if let Some(s) = v.is_str() {
                    s.to_uppercase()
                } else if let Some(id) = v.try_symbol() {
                    id.get_name().to_uppercase()
                } else {
                    return Err(MonorubyErr::typeerr("bad shutdown argument".to_string()));
                };
                match name.trim_start_matches("SHUT_") {
                    "RD" => libc::SHUT_RD,
                    "WR" => libc::SHUT_WR,
                    "RDWR" => libc::SHUT_RDWR,
                    _ => {
                        return Err(socket_error(
                            &globals.store,
                            format!("unknown shutdown argument: {name}"),
                        ));
                    }
                }
            }
        }
    };
    let fd = lfp.self_val().as_io_inner().fileno()?;
    // SAFETY: shutdown(2) on our fd.
    if unsafe { libc::shutdown(fd, how) } < 0 {
        return Err(last_errno_err(&globals.store, "shutdown(2)"));
    }
    Ok(Value::integer(0))
}
