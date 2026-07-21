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
    let udpsocket = globals.define_class("UDPSocket", ipsocket, OBJECT_CLASS);
    let udp_id = udpsocket.id();
    let unixsocket = globals.define_class("UNIXSocket", basic, OBJECT_CLASS);
    let unix_id = unixsocket.id();
    let unixserver = globals.define_class("UNIXServer", unixsocket, OBJECT_CLASS);
    let usrv_id = unixserver.id();
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

    // Datagram/message primitives (park-and-retry blocking discipline).
    globals.define_builtin_func_with(basic_id, "recv", basic_recv, 1, 3, false);
    globals.define_builtin_func_with_kw(
        basic_id,
        "recv_nonblock",
        basic_recv_nonblock,
        1,
        3,
        false,
        &["exception"],
        false,
    );
    globals.define_builtin_func_with(basic_id, "send", basic_send, 2, 3, false);
    // Same native under an internal name so stdlib/socket.rb can wrap
    // `send` (Addrinfo destination coercion) without recursing.
    globals.define_builtin_func_with(basic_id, "__send_raw", basic_send, 2, 3, false);
    // Wrap an existing fd in the receiver socket class (Socket.for_fd,
    // UNIXSocket.for_fd, ... — overrides the generic IO.for_fd).
    globals.define_builtin_class_func(basic_id, "for_fd", socket_for_fd, 1);
    globals.define_builtin_func_with(basic_id, "__recvfrom_raw", recvfrom_raw, 3, 3, false);
    globals.define_builtin_func_with(basic_id, "__recvmsg_raw", recvmsg_raw, 3, 3, false);
    globals.define_builtin_func_with(basic_id, "__sendmsg_raw", sendmsg_raw, 4, 4, false);
    globals.define_builtin_func(basic_id, "__sockname_raw", sockname_raw, 0);
    globals.define_builtin_func(basic_id, "__peername_raw", peername_raw, 0);
    globals.define_builtin_func_with(basic_id, "__accept_raw", accept_raw, 1, 1, false);

    // Generic sockaddr-level Socket surface.
    globals.define_builtin_func(sock_id, "bind", socket_bind, 1);
    globals.define_builtin_func_with_kw(
        sock_id,
        "connect",
        socket_connect,
        1,
        1,
        false,
        &["timeout"],
        false,
    );
    globals.define_builtin_func(sock_id, "listen", socket_listen, 1);
    globals.define_builtin_class_func_with(sock_id, "pack_sockaddr_in", pack_sockaddr_in, 2, 2, false);
    globals.define_builtin_class_func_with(sock_id, "sockaddr_in", pack_sockaddr_in, 2, 2, false);
    globals.define_builtin_class_func(sock_id, "unpack_sockaddr_in", unpack_sockaddr_in, 1);
    globals.define_builtin_class_func(sock_id, "pack_sockaddr_un", pack_sockaddr_un, 1);
    globals.define_builtin_class_func(sock_id, "sockaddr_un", pack_sockaddr_un, 1);
    globals.define_builtin_class_func(sock_id, "unpack_sockaddr_un", unpack_sockaddr_un, 1);
    globals.define_builtin_class_func_with(sock_id, "pair", socket_pair, 2, 3, false);
    globals.define_builtin_class_func_with(sock_id, "socketpair", socket_pair, 2, 3, false);

    globals.define_builtin_func(tsrv_id, "sysaccept", tcp_server_sysaccept, 0);

    // UDP: real datagram sockets (previously a raising stub).
    globals.define_builtin_class_func_with(udp_id, "new", udp_socket_new, 0, 1, false);
    globals.define_builtin_class_func_with(udp_id, "open", udp_socket_new, 0, 1, false);
    globals.define_builtin_func(udp_id, "bind", udp_bind, 2);
    globals.define_builtin_func(udp_id, "connect", udp_connect, 2);
    globals.define_builtin_func_with(udp_id, "send", udp_send, 2, 4, false);

    // UNIX domain sockets (previously a raising stub).
    globals.define_builtin_class_func(unix_id, "new", unix_socket_new, 1);
    globals.define_builtin_class_func_with(unix_id, "pair", unix_pair, 0, 2, false);
    globals.define_builtin_class_func_with(unix_id, "socketpair", unix_pair, 0, 2, false);
    globals.define_builtin_func(unix_id, "path", unix_path, 0);
    globals.define_builtin_class_func(usrv_id, "new", unix_server_new, 1);
    globals.define_builtin_func(usrv_id, "listen", socket_listen, 1);
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

/// `socket(2)` with close-on-exec (and optionally non-blocking) set.
/// Linux sets both atomically via `SOCK_CLOEXEC` / `SOCK_NONBLOCK`;
/// macOS/BSD have neither flag, so fall back to `fcntl(2)` after
/// creation (a benign race in a green-thread runtime — nothing forks
/// between the two calls).
fn cloexec_socket(domain: i32, ty: i32, proto: i32, nonblock: bool) -> i32 {
    #[cfg(target_os = "linux")]
    {
        let mut ty = ty | libc::SOCK_CLOEXEC;
        if nonblock {
            ty |= libc::SOCK_NONBLOCK;
        }
        // SAFETY: plain socket(2).
        unsafe { libc::socket(domain, ty, proto) }
    }
    #[cfg(not(target_os = "linux"))]
    {
        // SAFETY: plain socket(2), then fcntl on the fd we just created.
        unsafe {
            let fd = libc::socket(domain, ty, proto);
            if fd >= 0 {
                libc::fcntl(fd, libc::F_SETFD, libc::FD_CLOEXEC);
                if nonblock {
                    let flags = libc::fcntl(fd, libc::F_GETFL);
                    if flags >= 0 {
                        libc::fcntl(fd, libc::F_SETFL, flags | libc::O_NONBLOCK);
                    }
                }
            }
            fd
        }
    }
}

/// `accept(2)` returning a close-on-exec, *blocking* connection fd.
/// Linux uses `accept4(2)`; macOS/BSD fall back to `accept(2)` +
/// `fcntl(2)`, also clearing `O_NONBLOCK` — BSD-family accepted fds
/// inherit the listener's non-blocking flag (Linux ones do not), and
/// the IO layer expects blocking-mode sockets.
fn cloexec_accept(fd: i32) -> i32 {
    #[cfg(target_os = "linux")]
    // SAFETY: accept4 on the caller's listener fd; out params unused (NULL).
    unsafe {
        libc::accept4(
            fd,
            std::ptr::null_mut(),
            std::ptr::null_mut(),
            libc::SOCK_CLOEXEC,
        )
    }
    #[cfg(not(target_os = "linux"))]
    // SAFETY: accept on the caller's listener fd; fcntl on the fresh fd.
    unsafe {
        let conn = libc::accept(fd, std::ptr::null_mut(), std::ptr::null_mut());
        if conn >= 0 {
            libc::fcntl(conn, libc::F_SETFD, libc::FD_CLOEXEC);
            let flags = libc::fcntl(conn, libc::F_GETFL);
            if flags >= 0 {
                libc::fcntl(conn, libc::F_SETFL, flags & !libc::O_NONBLOCK);
            }
        }
        conn
    }
}

/// A new non-blocking, close-on-exec TCP socket fd.
fn new_tcp_fd(store: &Store) -> Result<i32> {
    let fd = cloexec_socket(libc::AF_INET, libc::SOCK_STREAM, 0, true);
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
        let conn = cloexec_accept(fd);
        if conn >= 0 {
            // SAFETY: conn is a fresh, owned fd from accept.
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
    let fd = cloexec_socket(domain, stype, proto, false);
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

//
// Generic message-level socket surface: recv/send (+_nonblock), the raw
// primitives behind recvfrom/recvmsg/sendmsg/accept/local_address (the
// Ruby layer in stdlib/socket.rb wraps them into Addrinfo-shaped
// results), UDP and UNIX-domain sockets, and sockaddr pack/unpack.
//
// Blocking discipline is uniform with `accept`/`connect` above: every
// potentially blocking attempt is issued non-blocking (`MSG_DONTWAIT`,
// or an O_NONBLOCK listener fd), and a would-block result parks the
// calling green thread on the scheduler's fd poller via `park_on_fd` —
// so `Thread#status` reports `"sleep"` while blocked (ruby/spec's
// `block_caller` matcher spins on exactly that).
//

/// Retry a would-block-signalling syscall attempt around the scheduler.
/// `attempt(fd)` returns `Ok(v)` on success or `Err(errno)`; `EAGAIN`
/// parks on `events` (after a poll point — signals / preemption /
/// `Thread#kill` land there), `EINTR` just re-runs the poll point, and
/// anything else surfaces as the matching `Errno::E*`. Closed-ness is
/// re-checked every round so a cross-thread `close` wakes the parked
/// thread into an IOError, mirroring `TCPServer#accept`.
fn park_retry<T>(
    vm: &mut Executor,
    globals: &mut Globals,
    io: Value,
    events: i16,
    ctx: &str,
    mut attempt: impl FnMut(i32) -> std::result::Result<T, i32>,
) -> Result<T> {
    let mut parked = false;
    loop {
        if io.as_io_inner().is_closed() {
            return Err(MonorubyErr::ioerr(if parked {
                "stream closed in another thread"
            } else {
                "closed stream"
            }));
        }
        let fd = io.as_io_inner().fileno()?;
        match attempt(fd) {
            Ok(v) => return Ok(v),
            Err(errno) => match errno {
                libc::EAGAIN | libc::EINTR => {
                    if crate::executor::execute_gc(vm, globals).is_none() {
                        return Err(vm.take_error());
                    }
                    if errno == libc::EAGAIN {
                        parked = true;
                        park_on_fd(vm, globals, fd, events, None)?;
                    }
                }
                _ => return Err(errno_err(&globals.store, errno, ctx)),
            },
        }
    }
}

fn last_errno() -> i32 {
    std::io::Error::last_os_error()
        .raw_os_error()
        .unwrap_or(libc::EIO)
}

/// One `recvfrom(2)` attempt (non-blocking via `MSG_DONTWAIT`).
/// Returns `(data, sender_sockaddr_bytes)`; the sockaddr is empty for
/// connected stream sockets.
fn recvfrom_attempt(fd: i32, maxlen: usize, flags: i32) -> std::result::Result<(Vec<u8>, Vec<u8>), i32> {
    let mut buf = vec![0u8; maxlen.max(1)];
    let mut sa = [0u8; 128];
    let mut sa_len = sa.len() as libc::socklen_t;
    // SAFETY: buf/sa are live locals sized to the lengths passed.
    let n = unsafe {
        libc::recvfrom(
            fd,
            buf.as_mut_ptr() as *mut libc::c_void,
            maxlen,
            flags | libc::MSG_DONTWAIT,
            sa.as_mut_ptr() as *mut libc::sockaddr,
            &mut sa_len,
        )
    };
    if n < 0 {
        return Err(last_errno());
    }
    buf.truncate(n as usize);
    Ok((buf, sa[..sa_len as usize].to_vec()))
}

/// Whether `fd` is a stream-type socket (`SO_TYPE == SOCK_STREAM`).
/// A zero-byte `recvfrom(2)` on a stream socket means EOF (peer
/// shutdown) and surfaces as `nil` in Ruby, while on a datagram socket
/// it is a legitimate empty datagram (`""`).
fn is_stream_socket(fd: i32) -> bool {
    let mut ty: i32 = 0;
    let mut len = std::mem::size_of::<i32>() as libc::socklen_t;
    // SAFETY: getsockopt out-params sized to an i32.
    let rc = unsafe {
        libc::getsockopt(
            fd,
            libc::SOL_SOCKET,
            libc::SO_TYPE,
            &mut ty as *mut i32 as *mut libc::c_void,
            &mut len,
        )
    };
    rc == 0 && ty == libc::SOCK_STREAM
}

/// Write `data` into the supplied output buffer (preserving its identity
/// and encoding) or build a fresh binary string.
fn data_to_result(data: Vec<u8>, outbuf: Option<Value>) -> Value {
    match outbuf {
        Some(mut v) => {
            let enc = v.as_rstring_inner().encoding();
            *v.as_rstring_inner_mut() = RStringInner::from_encoding(&data, enc);
            v
        }
        None => Value::bytes(data),
    }
}

/// The `(maxlen, flags = 0, outbuf = nil)` argument triple shared by
/// `recv` / `recv_nonblock`.
fn recv_args(vm: &mut Executor, globals: &mut Globals, lfp: &Lfp) -> Result<(usize, i32, Option<Value>)> {
    let maxlen = lfp.arg(0).coerce_to_int_i64(vm, globals)?;
    if maxlen < 0 {
        return Err(MonorubyErr::argumenterr(format!("negative length {} given", maxlen)));
    }
    let flags = match lfp.try_arg(1) {
        Some(v) if !v.is_nil() => v.coerce_to_i64(&globals.store)? as i32,
        _ => 0,
    };
    let outbuf = match lfp.try_arg(2) {
        Some(v) if !v.is_nil() => Some(v.coerce_to_rstring(vm, globals)?.as_val()),
        _ => None,
    };
    Ok((maxlen as usize, flags, outbuf))
}

///
/// ### BasicSocket#recv
///
/// - recv(maxlen, flags = 0, outbuf = nil) -> String
///
/// Parks the calling green thread until data (or EOF) arrives.
///
/// [https://docs.ruby-lang.org/ja/latest/method/BasicSocket/i/recv.html]
#[monoruby_builtin]
fn basic_recv(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let (maxlen, flags, outbuf) = recv_args(vm, globals, &lfp)?;
    let (data, _) = park_retry(vm, globals, lfp.self_val(), libc::POLLIN, "recvfrom(2)", |fd| {
        recvfrom_attempt(fd, maxlen, flags)
    })?;
    if data.is_empty() && maxlen > 0 && is_stream_socket(lfp.self_val().as_io_inner().fileno()?) {
        return Ok(Value::nil());
    }
    Ok(data_to_result(data, outbuf))
}

///
/// ### BasicSocket#recv_nonblock
///
/// - recv_nonblock(maxlen, flags = 0, outbuf = nil, exception: true) -> String | :wait_readable
///
#[monoruby_builtin]
fn basic_recv_nonblock(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let (maxlen, flags, outbuf) = recv_args(vm, globals, &lfp)?;
    let exception = lfp.try_arg(3).map_or(true, |v| v.as_bool());
    if lfp.self_val().as_io_inner().is_closed() {
        return Err(MonorubyErr::ioerr("closed stream"));
    }
    let fd = lfp.self_val().as_io_inner().fileno()?;
    match recvfrom_attempt(fd, maxlen, flags) {
        Ok((data, _)) => Ok(data_to_result(data, outbuf)),
        Err(errno) if errno == libc::EAGAIN => {
            if exception {
                let cid = super::io::io_wait_class(globals, "EAGAINWaitReadable")
                    .ok_or_else(|| MonorubyErr::ioerr("IO::EAGAINWaitReadable not defined"))?;
                Err(MonorubyErr::new(
                    MonorubyErrKind::Other(cid),
                    "Resource temporarily unavailable - recvfrom(2) would block".to_string(),
                ))
            } else {
                Ok(Value::symbol(IdentId::get_id("wait_readable")))
            }
        }
        Err(errno) => Err(errno_err(&globals.store, errno, "recvfrom(2)")),
    }
}

///
/// ### BasicSocket#send
///
/// - send(mesg, flags, dest_sockaddr = nil) -> Integer
///
/// Parks on POLLOUT when the kernel send buffer is full. An unconnected
/// datagram socket without `dest_sockaddr` surfaces
/// `Errno::EDESTADDRREQ`, as CRuby does.
///
/// [https://docs.ruby-lang.org/ja/latest/method/BasicSocket/i/send.html]
#[monoruby_builtin]
fn basic_send(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let mesg = lfp.arg(0).coerce_to_rstring(vm, globals)?.as_val();
    let bytes = mesg.as_rstring_inner().as_bytes().to_vec();
    let flags = lfp.arg(1).coerce_to_i64(&globals.store)? as i32;
    let dest: Option<Vec<u8>> = match lfp.try_arg(2) {
        Some(v) if !v.is_nil() => match v.is_rstring_inner() {
            Some(s) => Some(s.as_bytes().to_vec()),
            None => {
                return Err(MonorubyErr::typeerr(
                    "dest_sockaddr should be a packed sockaddr String".to_string(),
                ));
            }
        },
        _ => None,
    };
    let n = park_retry(vm, globals, lfp.self_val(), libc::POLLOUT, "sendto(2)", |fd| {
        // SAFETY: bytes/dest are live locals sized to the lengths passed.
        let n = unsafe {
            match &dest {
                Some(sa) => libc::sendto(
                    fd,
                    bytes.as_ptr() as *const libc::c_void,
                    bytes.len(),
                    flags | libc::MSG_DONTWAIT,
                    sa.as_ptr() as *const libc::sockaddr,
                    sa.len() as libc::socklen_t,
                ),
                None => libc::send(
                    fd,
                    bytes.as_ptr() as *const libc::c_void,
                    bytes.len(),
                    flags | libc::MSG_DONTWAIT,
                ),
            }
        };
        if n < 0 { Err(last_errno()) } else { Ok(n) }
    })?;
    Ok(Value::integer(n as i64))
}

///
/// ### BasicSocket#__recvfrom_raw (stdlib/socket.rb internal)
///
/// - __recvfrom_raw(maxlen, flags, blocking) -> [String, sockaddr_bytes]
///
/// The primitive behind `Socket#recvfrom` / `IPSocket#recvfrom` /
/// `UNIXSocket#recvfrom` and their `_nonblock` variants; the Ruby layer
/// shapes the sender address.
#[monoruby_builtin]
fn recvfrom_raw(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let maxlen = lfp.arg(0).coerce_to_int_i64(vm, globals)?.max(0) as usize;
    let flags = lfp.arg(1).coerce_to_i64(&globals.store)? as i32;
    let blocking = lfp.arg(2).as_bool();
    let (data, sa) = if blocking {
        park_retry(vm, globals, lfp.self_val(), libc::POLLIN, "recvfrom(2)", |fd| {
            recvfrom_attempt(fd, maxlen, flags)
        })?
    } else {
        let fd = lfp.self_val().as_io_inner().fileno()?;
        match recvfrom_attempt(fd, maxlen, flags) {
            Ok(v) => v,
            Err(errno) if errno == libc::EAGAIN => {
                let cid = super::io::io_wait_class(globals, "EAGAINWaitReadable")
                    .ok_or_else(|| MonorubyErr::ioerr("IO::EAGAINWaitReadable not defined"))?;
                return Err(MonorubyErr::new(
                    MonorubyErrKind::Other(cid),
                    "Resource temporarily unavailable - recvfrom(2) would block".to_string(),
                ));
            }
            Err(errno) => return Err(errno_err(&globals.store, errno, "recvfrom(2)")),
        }
    };
    if data.is_empty() && maxlen > 0 && is_stream_socket(lfp.self_val().as_io_inner().fileno()?) {
        return Ok(Value::nil());
    }
    Ok(Value::array_from_vec(vec![Value::bytes(data), Value::bytes(sa)]))
}

/// One `recvmsg(2)` attempt (non-blocking via `MSG_DONTWAIT`).
fn recvmsg_attempt(fd: i32, maxlen: usize, flags: i32) -> std::result::Result<(Vec<u8>, Vec<u8>, i32), i32> {
    let mut buf = vec![0u8; maxlen.max(1)];
    let mut sa = [0u8; 128];
    let mut iov = libc::iovec {
        iov_base: buf.as_mut_ptr() as *mut libc::c_void,
        iov_len: maxlen,
    };
    // SAFETY: msghdr is plain-old-data; zeroing is a valid initial state.
    let mut msg: libc::msghdr = unsafe { std::mem::zeroed() };
    msg.msg_name = sa.as_mut_ptr() as *mut libc::c_void;
    msg.msg_namelen = sa.len() as libc::socklen_t;
    msg.msg_iov = &mut iov;
    msg.msg_iovlen = 1;
    // SAFETY: msg points at live locals set up above.
    let n = unsafe { libc::recvmsg(fd, &mut msg, flags | libc::MSG_DONTWAIT) };
    if n < 0 {
        return Err(last_errno());
    }
    buf.truncate(n as usize);
    Ok((buf, sa[..msg.msg_namelen as usize].to_vec(), msg.msg_flags))
}

///
/// ### BasicSocket#__recvmsg_raw (stdlib/socket.rb internal)
///
/// - __recvmsg_raw(maxlen, flags, blocking) -> [String, sockaddr_bytes, rflags]
///
#[monoruby_builtin]
fn recvmsg_raw(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let maxlen = lfp.arg(0).coerce_to_int_i64(vm, globals)?.max(0) as usize;
    let flags = lfp.arg(1).coerce_to_i64(&globals.store)? as i32;
    let blocking = lfp.arg(2).as_bool();
    let (data, sa, rflags) = if blocking {
        park_retry(vm, globals, lfp.self_val(), libc::POLLIN, "recvmsg(2)", |fd| {
            recvmsg_attempt(fd, maxlen, flags)
        })?
    } else {
        let fd = lfp.self_val().as_io_inner().fileno()?;
        match recvmsg_attempt(fd, maxlen, flags) {
            Ok(v) => v,
            Err(errno) if errno == libc::EAGAIN => {
                let cid = super::io::io_wait_class(globals, "EAGAINWaitReadable")
                    .ok_or_else(|| MonorubyErr::ioerr("IO::EAGAINWaitReadable not defined"))?;
                return Err(MonorubyErr::new(
                    MonorubyErrKind::Other(cid),
                    "Resource temporarily unavailable - recvmsg(2) would block".to_string(),
                ));
            }
            Err(errno) => return Err(errno_err(&globals.store, errno, "recvmsg(2)")),
        }
    };
    if data.is_empty() && maxlen > 0 && is_stream_socket(lfp.self_val().as_io_inner().fileno()?) {
        return Ok(Value::nil());
    }
    Ok(Value::array_from_vec(vec![
        Value::bytes(data),
        Value::bytes(sa),
        Value::integer(rflags as i64),
    ]))
}

///
/// ### BasicSocket#__sendmsg_raw (stdlib/socket.rb internal)
///
/// - __sendmsg_raw(mesg, flags, dest_sockaddr_or_nil, blocking) -> Integer
///
#[monoruby_builtin]
fn sendmsg_raw(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let mesg = lfp.arg(0).coerce_to_rstring(vm, globals)?.as_val();
    let bytes = mesg.as_rstring_inner().as_bytes().to_vec();
    let flags = lfp.arg(1).coerce_to_i64(&globals.store)? as i32;
    let dest: Option<Vec<u8>> = match lfp.try_arg(2) {
        Some(v) if !v.is_nil() => match v.is_rstring_inner() {
            Some(s) => Some(s.as_bytes().to_vec()),
            None => {
                return Err(MonorubyErr::typeerr(
                    "dest_sockaddr should be a packed sockaddr String".to_string(),
                ));
            }
        },
        _ => None,
    };
    let blocking = lfp.arg(3).as_bool();
    let attempt = |fd: i32| -> std::result::Result<isize, i32> {
        let mut iov = libc::iovec {
            iov_base: bytes.as_ptr() as *mut libc::c_void,
            iov_len: bytes.len(),
        };
        // SAFETY: msghdr is plain-old-data; zeroing is a valid initial state.
        let mut msg: libc::msghdr = unsafe { std::mem::zeroed() };
        if let Some(sa) = &dest {
            msg.msg_name = sa.as_ptr() as *mut libc::c_void;
            msg.msg_namelen = sa.len() as libc::socklen_t;
        }
        msg.msg_iov = &mut iov;
        msg.msg_iovlen = 1;
        // SAFETY: msg points at live locals set up above.
        let n = unsafe { libc::sendmsg(fd, &msg, flags | libc::MSG_DONTWAIT) };
        if n < 0 { Err(last_errno()) } else { Ok(n) }
    };
    let n = if blocking {
        park_retry(vm, globals, lfp.self_val(), libc::POLLOUT, "sendmsg(2)", attempt)?
    } else {
        let fd = lfp.self_val().as_io_inner().fileno()?;
        match attempt(fd) {
            Ok(n) => n,
            Err(errno) if errno == libc::EAGAIN => {
                let cid = super::io::io_wait_class(globals, "EAGAINWaitWritable")
                    .ok_or_else(|| MonorubyErr::ioerr("IO::EAGAINWaitWritable not defined"))?;
                return Err(MonorubyErr::new(
                    MonorubyErrKind::Other(cid),
                    "Resource temporarily unavailable - sendmsg(2) would block".to_string(),
                ));
            }
            Err(errno) => return Err(errno_err(&globals.store, errno, "sendmsg(2)")),
        }
    };
    Ok(Value::integer(n as i64))
}

/// Generic getsockname/getpeername returning the raw sockaddr bytes.
fn name_raw(store: &Store, io: Value, peer: bool) -> Result<Value> {
    let fd = io.as_io_inner().fileno()?;
    let mut sa = [0u8; 128];
    let mut len = sa.len() as libc::socklen_t;
    // SAFETY: out-params sized to the local buffer.
    let rc = unsafe {
        let p = sa.as_mut_ptr() as *mut libc::sockaddr;
        if peer {
            libc::getpeername(fd, p, &mut len)
        } else {
            libc::getsockname(fd, p, &mut len)
        }
    };
    if rc < 0 {
        return Err(last_errno_err(store, if peer { "getpeername(2)" } else { "getsockname(2)" }));
    }
    Ok(Value::bytes(sa[..len as usize].to_vec()))
}

/// ### BasicSocket#__sockname_raw (stdlib/socket.rb internal)
#[monoruby_builtin]
fn sockname_raw(_: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    name_raw(&globals.store, lfp.self_val(), false)
}

/// ### BasicSocket#__peername_raw (stdlib/socket.rb internal)
#[monoruby_builtin]
fn peername_raw(_: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    name_raw(&globals.store, lfp.self_val(), true)
}

/// Put `fd` in non-blocking mode (sticky — every blocking path here
/// parks around EAGAIN, so the mode is transparent to Ruby code).
fn set_nonblocking(fd: i32) {
    // SAFETY: fcntl on an fd we own; best-effort.
    unsafe {
        let flags = libc::fcntl(fd, libc::F_GETFL);
        if flags >= 0 {
            libc::fcntl(fd, libc::F_SETFL, flags | libc::O_NONBLOCK);
        }
    }
}

/// `accept(2)` capturing the peer sockaddr; the new fd is made
/// close-on-exec and blocking (see `cloexec_accept`).
fn accept_addr_attempt(fd: i32) -> std::result::Result<(i32, Vec<u8>), i32> {
    let mut sa = [0u8; 128];
    let mut len = sa.len() as libc::socklen_t;
    // SAFETY: accept on the caller's listener fd with length-matched
    // out-params; fcntl on the fresh fd.
    let conn = unsafe {
        let conn = libc::accept(fd, sa.as_mut_ptr() as *mut libc::sockaddr, &mut len);
        if conn >= 0 {
            libc::fcntl(conn, libc::F_SETFD, libc::FD_CLOEXEC);
            let flags = libc::fcntl(conn, libc::F_GETFL);
            if flags >= 0 {
                libc::fcntl(conn, libc::F_SETFL, flags & !libc::O_NONBLOCK);
            }
        }
        conn
    };
    if conn < 0 {
        return Err(last_errno());
    }
    Ok((conn, sa[..len as usize].to_vec()))
}

///
/// ### BasicSocket#__accept_raw (stdlib/socket.rb internal)
///
/// - __accept_raw(klass_or_nil) -> [Socket-like, sockaddr_bytes] | [Integer, sockaddr_bytes]
///
/// The blocking primitive behind `Socket#accept` / `UNIXServer#accept`
/// and their `sysaccept` variants: parks until a connection arrives,
/// then wraps the fd in an instance of `klass` (or returns the raw fd
/// when `klass` is nil). The listener fd is switched to non-blocking on
/// first use, like `TCPServer`'s permanently non-blocking listener.
#[monoruby_builtin]
fn accept_raw(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_ = lfp.self_val();
    let klass = lfp.arg(0);
    set_nonblocking(self_.as_io_inner().fileno()?);
    let (conn, sa) = park_retry(vm, globals, self_, libc::POLLIN, "accept(2)", |fd| {
        match accept_addr_attempt(fd) {
            // ECONNABORTED: the peer went away between the poll wake and
            // the accept — just wait for the next connection.
            Err(errno) if errno == libc::ECONNABORTED => Err(libc::EINTR),
            other => other,
        }
    })?;
    let first = if klass.is_nil() {
        Value::integer(conn as i64)
    } else {
        let class_id = klass.as_class().id();
        let name = globals.store.get_class_name(class_id);
        // SAFETY: conn is a fresh, owned fd from accept.
        let file = unsafe { std::fs::File::from_raw_fd(conn) };
        Value::new_socket(file, name, class_id)
    };
    Ok(Value::array_from_vec(vec![first, Value::bytes(sa)]))
}

///
/// ### BasicSocket.for_fd
///
/// - for_fd(fd) -> instance of the receiver class
///
/// Takes ownership of an existing socket fd (e.g. one returned by
/// `sysaccept`) and wraps it in the receiver class.
#[monoruby_builtin]
fn socket_for_fd(_: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let fd = lfp.arg(0).coerce_to_i64(&globals.store)? as i32;
    if fd < 0 {
        return Err(MonorubyErr::argumenterr(format!("invalid fd: {fd}")));
    }
    let class_id = lfp.self_val().as_class().id();
    let name = globals.store.get_class_name(class_id);
    // SAFETY: per for_fd's contract the caller transfers ownership of fd.
    let file = unsafe { std::fs::File::from_raw_fd(fd) };
    Ok(Value::new_socket(file, name, class_id))
}

///
/// ### TCPServer#sysaccept
///
/// - sysaccept -> Integer
///
#[monoruby_builtin]
fn tcp_server_sysaccept(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_ = lfp.self_val();
    let (conn, _) = park_retry(vm, globals, self_, libc::POLLIN, "accept(2)", |fd| {
        match accept_addr_attempt(fd) {
            Err(errno) if errno == libc::ECONNABORTED => Err(libc::EINTR),
            other => other,
        }
    })?;
    Ok(Value::integer(conn as i64))
}

///
/// ### Socket#bind
///
/// - bind(sockaddr) -> 0
///
#[monoruby_builtin]
fn socket_bind(_: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let sa = match lfp.arg(0).is_rstring_inner() {
        Some(s) => s.as_bytes().to_vec(),
        None => {
            return Err(MonorubyErr::typeerr(
                "bind argument should be a packed sockaddr String".to_string(),
            ));
        }
    };
    let fd = lfp.self_val().as_io_inner().fileno()?;
    // SAFETY: bind(2) with a length-matched byte buffer.
    if unsafe { libc::bind(fd, sa.as_ptr() as *const libc::sockaddr, sa.len() as libc::socklen_t) } < 0 {
        return Err(last_errno_err(&globals.store, "bind(2)"));
    }
    Ok(Value::integer(0))
}

/// Park until a non-blocking `connect(2)` on `fd` settles, then check
/// `SO_ERROR` (the same discipline as `TCPSocket.new`).
fn wait_connect_settled(
    vm: &mut Executor,
    globals: &mut Globals,
    fd: i32,
    deadline: Option<std::time::Instant>,
    ctx: &str,
) -> Result<()> {
    loop {
        park_on_fd(vm, globals, fd, libc::POLLOUT, deadline)?;
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
        if soerr != 0 {
            return Err(errno_err(&globals.store, soerr, ctx));
        }
        let mut pfd = libc::pollfd {
            fd,
            events: libc::POLLOUT,
            revents: 0,
        };
        // SAFETY: one pollfd, length 1, timeout 0.
        let ready = unsafe { libc::poll(&mut pfd, 1, 0) };
        if ready > 0 {
            return Ok(());
        }
        if let Some(dl) = deadline
            && std::time::Instant::now() >= dl
        {
            return Err(errno_err(&globals.store, libc::ETIMEDOUT, ctx));
        }
    }
}

///
/// ### Socket#connect
///
/// - connect(sockaddr, timeout: nil) -> 0
///
/// Non-blocking connect parking on POLLOUT, like `TCPSocket.new`.
#[monoruby_builtin]
fn socket_connect(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let sa = match lfp.arg(0).is_rstring_inner() {
        Some(s) => s.as_bytes().to_vec(),
        None => {
            return Err(MonorubyErr::typeerr(
                "connect argument should be a packed sockaddr String".to_string(),
            ));
        }
    };
    let deadline = match lfp.try_arg(1) {
        Some(v) if !v.is_nil() => {
            let secs = v.coerce_to_f64(vm, globals)?;
            Some(std::time::Instant::now() + std::time::Duration::from_secs_f64(secs.max(0.0)))
        }
        _ => None,
    };
    let fd = lfp.self_val().as_io_inner().fileno()?;
    set_nonblocking(fd);
    // SAFETY: connect(2) with a length-matched byte buffer.
    let rc = unsafe { libc::connect(fd, sa.as_ptr() as *const libc::sockaddr, sa.len() as libc::socklen_t) };
    if rc < 0 {
        let errno = last_errno();
        match errno {
            libc::EINPROGRESS | libc::EINTR | libc::EAGAIN => {
                wait_connect_settled(vm, globals, fd, deadline, "connect(2)")?;
            }
            _ => {
                set_blocking(fd);
                return Err(errno_err(&globals.store, errno, "connect(2)"));
            }
        }
    }
    set_blocking(fd);
    Ok(Value::integer(0))
}

///
/// ### Socket.pack_sockaddr_in / Socket.sockaddr_in
///
/// - pack_sockaddr_in(port, host) -> String
///
#[monoruby_builtin]
fn pack_sockaddr_in(_: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let port = match lfp.arg(0).try_fixnum() {
        Some(i) if (0..=65535).contains(&i) => i as u16,
        _ => value_to_port(globals, lfp.arg(0))?,
    };
    let host = value_to_host(globals, lfp.arg(1))?;
    let addr = resolve_ipv4(&globals.store, host.as_deref(), true)?;
    let sin = sockaddr_in(addr, port);
    // SAFETY: viewing a POD struct as bytes.
    let bytes = unsafe {
        std::slice::from_raw_parts(
            &sin as *const libc::sockaddr_in as *const u8,
            std::mem::size_of::<libc::sockaddr_in>(),
        )
    };
    Ok(Value::bytes(bytes.to_vec()))
}

/// Read a `sockaddr_in` out of packed bytes, validating family.
fn sockaddr_in_from_bytes(bytes: &[u8]) -> Result<libc::sockaddr_in> {
    if bytes.len() < std::mem::size_of::<libc::sockaddr_in>() {
        return Err(MonorubyErr::argumenterr("not an AF_INET sockaddr".to_string()));
    }
    // SAFETY: length checked; sockaddr_in is plain-old-data.
    let sin: libc::sockaddr_in =
        unsafe { std::ptr::read_unaligned(bytes.as_ptr() as *const libc::sockaddr_in) };
    if sin.sin_family != libc::AF_INET as libc::sa_family_t {
        return Err(MonorubyErr::argumenterr("not an AF_INET sockaddr".to_string()));
    }
    Ok(sin)
}

///
/// ### Socket.unpack_sockaddr_in
///
/// - unpack_sockaddr_in(sockaddr) -> [port, ip_address]
///
#[monoruby_builtin]
fn unpack_sockaddr_in(_: &mut Executor, _globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let v = lfp.arg(0);
    let bytes = v
        .is_rstring_inner()
        .ok_or_else(|| MonorubyErr::typeerr("sockaddr should be a String".to_string()))?
        .as_bytes()
        .to_vec();
    let sin = sockaddr_in_from_bytes(&bytes)?;
    let ip = std::net::Ipv4Addr::from(u32::from_be(sin.sin_addr.s_addr)).to_string();
    Ok(Value::array_from_vec(vec![
        Value::integer(u16::from_be(sin.sin_port) as i64),
        Value::string(ip),
    ]))
}

/// Longest path that fits `sun_path` with a trailing NUL.
const UNIX_PATH_MAX: usize = 107;

fn unix_path_bytes(globals: &Globals, v: Value) -> Result<Vec<u8>> {
    let path = match v.is_str() {
        Some(s) => s.to_string(),
        None => {
            return Err(MonorubyErr::typeerr(format!(
                "no implicit conversion of {} into String",
                globals.store.get_class_name(v.class())
            )));
        }
    };
    if path.as_bytes().contains(&0) {
        return Err(MonorubyErr::argumenterr("path name contains null byte".to_string()));
    }
    if path.len() > UNIX_PATH_MAX {
        return Err(MonorubyErr::argumenterr(format!(
            "too long unix socket path ({} bytes given but {} bytes max)",
            path.len(),
            UNIX_PATH_MAX
        )));
    }
    Ok(path.into_bytes())
}

fn sockaddr_un_from_path(path: &[u8]) -> libc::sockaddr_un {
    // SAFETY: sockaddr_un is plain-old-data; zeroing is a valid initial state.
    let mut sun: libc::sockaddr_un = unsafe { std::mem::zeroed() };
    sun.sun_family = libc::AF_UNIX as libc::sa_family_t;
    for (i, b) in path.iter().enumerate() {
        sun.sun_path[i] = *b as libc::c_char;
    }
    sun
}

///
/// ### Socket.pack_sockaddr_un / Socket.sockaddr_un
///
/// - pack_sockaddr_un(path) -> String
///
#[monoruby_builtin]
fn pack_sockaddr_un(_: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let path = unix_path_bytes(globals, lfp.arg(0))?;
    let sun = sockaddr_un_from_path(&path);
    // SAFETY: viewing a POD struct as bytes.
    let bytes = unsafe {
        std::slice::from_raw_parts(
            &sun as *const libc::sockaddr_un as *const u8,
            std::mem::size_of::<libc::sockaddr_un>(),
        )
    };
    Ok(Value::bytes(bytes.to_vec()))
}

///
/// ### Socket.unpack_sockaddr_un
///
/// - unpack_sockaddr_un(sockaddr) -> String
///
#[monoruby_builtin]
fn unpack_sockaddr_un(_: &mut Executor, _globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let v = lfp.arg(0);
    let bytes = v
        .is_rstring_inner()
        .ok_or_else(|| MonorubyErr::typeerr("sockaddr should be a String".to_string()))?
        .as_bytes()
        .to_vec();
    let path = sun_path_from_bytes(&bytes)
        .ok_or_else(|| MonorubyErr::argumenterr("not an AF_UNIX sockaddr".to_string()))?;
    Ok(Value::string_from_vec(path))
}

/// Extract the NUL-terminated `sun_path` out of packed AF_UNIX sockaddr
/// bytes; `None` when the family doesn't match.
fn sun_path_from_bytes(bytes: &[u8]) -> Option<Vec<u8>> {
    let path_off = std::mem::offset_of!(libc::sockaddr_un, sun_path);
    if bytes.len() < path_off {
        return None;
    }
    // SAFETY: header length checked; sockaddr_un is plain-old-data and a
    // short input only leaves the zeroed tail in place.
    let sun: libc::sockaddr_un = unsafe {
        let mut tmp: libc::sockaddr_un = std::mem::zeroed();
        std::ptr::copy_nonoverlapping(
            bytes.as_ptr(),
            &mut tmp as *mut libc::sockaddr_un as *mut u8,
            bytes.len().min(std::mem::size_of::<libc::sockaddr_un>()),
        );
        tmp
    };
    if sun.sun_family != libc::AF_UNIX as libc::sa_family_t {
        return None;
    }
    let path: Vec<u8> = sun
        .sun_path
        .iter()
        .take(bytes.len().saturating_sub(path_off))
        .take_while(|&&c| c != 0)
        .map(|&c| c as u8)
        .collect();
    Some(path)
}

/// Make both socketpair fds close-on-exec.
fn cloexec_pair(fds: [i32; 2]) {
    for fd in fds {
        // SAFETY: fcntl on fds we own; best-effort.
        unsafe {
            libc::fcntl(fd, libc::F_SETFD, libc::FD_CLOEXEC);
        }
    }
}

///
/// ### Socket.pair / Socket.socketpair
///
/// - pair(family, type, protocol = 0) -> [Socket, Socket]
///
#[monoruby_builtin]
fn socket_pair(_: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let family = family_const(&globals.store, lfp.arg(0))?;
    let stype = type_const(&globals.store, lfp.arg(1))?;
    let proto = match lfp.try_arg(2) {
        Some(v) if !v.is_nil() => v.coerce_to_i64(&globals.store)? as i32,
        _ => 0,
    };
    make_pair(globals, family, stype, proto, lfp.self_val().as_class().id())
}

///
/// ### UNIXSocket.pair / UNIXSocket.socketpair
///
/// - pair(type = :STREAM, protocol = 0) -> [UNIXSocket, UNIXSocket]
///
#[monoruby_builtin]
fn unix_pair(_: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let stype = match lfp.try_arg(0) {
        Some(v) if !v.is_nil() => type_const(&globals.store, v)?,
        _ => libc::SOCK_STREAM,
    };
    let proto = match lfp.try_arg(1) {
        Some(v) if !v.is_nil() => v.coerce_to_i64(&globals.store)? as i32,
        _ => 0,
    };
    make_pair(globals, libc::AF_UNIX, stype, proto, lfp.self_val().as_class().id())
}

fn make_pair(globals: &Globals, family: i32, stype: i32, proto: i32, class_id: ClassId) -> Result<Value> {
    let mut fds = [0i32; 2];
    // SAFETY: socketpair(2) with a two-slot out array.
    if unsafe { libc::socketpair(family, stype, proto, fds.as_mut_ptr()) } < 0 {
        return Err(last_errno_err(&globals.store, "socketpair(2)"));
    }
    cloexec_pair(fds);
    let name = globals.store.get_class_name(class_id);
    // SAFETY: both fds are fresh, owned socketpair fds.
    let (a, b) = unsafe {
        (
            Value::new_socket(std::fs::File::from_raw_fd(fds[0]), name.clone(), class_id),
            Value::new_socket(std::fs::File::from_raw_fd(fds[1]), name, class_id),
        )
    };
    Ok(Value::array_from_vec(vec![a, b]))
}

///
/// ### UDPSocket.new
///
/// - new(address_family = Socket::AF_INET) -> UDPSocket
///
#[monoruby_builtin]
fn udp_socket_new(_: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let family = match lfp.try_arg(0) {
        Some(v) if !v.is_nil() => family_const(&globals.store, v)?,
        _ => libc::AF_INET,
    };
    let fd = cloexec_socket(family, libc::SOCK_DGRAM, 0, false);
    if fd < 0 {
        return Err(last_errno_err(&globals.store, "socket(2)"));
    }
    let class_id = lfp.self_val().as_class().id();
    // SAFETY: fd is a fresh, owned socket fd.
    let file = unsafe { std::fs::File::from_raw_fd(fd) };
    Ok(Value::new_socket(file, "UDPSocket".into(), class_id))
}

///
/// ### UDPSocket#bind
///
/// - bind(host, port) -> 0
///
#[monoruby_builtin]
fn udp_bind(_: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let host = value_to_host(globals, lfp.arg(0))?;
    let port = value_to_port(globals, lfp.arg(1))?;
    let addr = resolve_ipv4(&globals.store, host.as_deref(), true)?;
    let sin = sockaddr_in(addr, port);
    let fd = lfp.self_val().as_io_inner().fileno()?;
    // SAFETY: bind(2) with a valid sockaddr_in.
    if unsafe {
        libc::bind(
            fd,
            &sin as *const libc::sockaddr_in as *const libc::sockaddr,
            std::mem::size_of::<libc::sockaddr_in>() as libc::socklen_t,
        )
    } < 0
    {
        return Err(last_errno_err(&globals.store, "bind(2)"));
    }
    Ok(Value::integer(0))
}

///
/// ### UDPSocket#connect
///
/// - connect(host, port) -> 0
///
/// Datagram connect just records the default peer — no handshake, so no
/// parking is needed.
#[monoruby_builtin]
fn udp_connect(_: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let host = value_to_host(globals, lfp.arg(0))?;
    let port = value_to_port(globals, lfp.arg(1))?;
    let addr = resolve_ipv4(&globals.store, host.as_deref(), false)?;
    let sin = sockaddr_in(addr, port);
    let fd = lfp.self_val().as_io_inner().fileno()?;
    // SAFETY: connect(2) with a valid sockaddr_in.
    if unsafe {
        libc::connect(
            fd,
            &sin as *const libc::sockaddr_in as *const libc::sockaddr,
            std::mem::size_of::<libc::sockaddr_in>() as libc::socklen_t,
        )
    } < 0
    {
        return Err(last_errno_err(&globals.store, "connect(2)"));
    }
    Ok(Value::integer(0))
}

///
/// ### UDPSocket#send
///
/// - send(mesg, flags) -> Integer                     (connected)
/// - send(mesg, flags, sockaddr_to) -> Integer
/// - send(mesg, flags, host, port) -> Integer
///
#[monoruby_builtin]
fn udp_send(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let mesg = lfp.arg(0).coerce_to_rstring(vm, globals)?.as_val();
    let bytes = mesg.as_rstring_inner().as_bytes().to_vec();
    let flags = lfp.arg(1).coerce_to_i64(&globals.store)? as i32;
    let dest: Option<Vec<u8>> = match (lfp.try_arg(2), lfp.try_arg(3)) {
        (Some(host), Some(port)) if !port.is_nil() => {
            let host = value_to_host(globals, host)?;
            let port = value_to_port(globals, port)?;
            let addr = resolve_ipv4(&globals.store, host.as_deref(), false)?;
            let sin = sockaddr_in(addr, port);
            // SAFETY: viewing a POD struct as bytes.
            let sa = unsafe {
                std::slice::from_raw_parts(
                    &sin as *const libc::sockaddr_in as *const u8,
                    std::mem::size_of::<libc::sockaddr_in>(),
                )
            };
            Some(sa.to_vec())
        }
        (Some(sa), _) if !sa.is_nil() => match sa.is_rstring_inner() {
            Some(s) => Some(s.as_bytes().to_vec()),
            None => {
                return Err(MonorubyErr::typeerr(
                    "sockaddr_to should be a packed sockaddr String".to_string(),
                ));
            }
        },
        _ => None,
    };
    let n = park_retry(vm, globals, lfp.self_val(), libc::POLLOUT, "sendto(2)", |fd| {
        // SAFETY: bytes/dest are live locals sized to the lengths passed.
        let n = unsafe {
            match &dest {
                Some(sa) => libc::sendto(
                    fd,
                    bytes.as_ptr() as *const libc::c_void,
                    bytes.len(),
                    flags | libc::MSG_DONTWAIT,
                    sa.as_ptr() as *const libc::sockaddr,
                    sa.len() as libc::socklen_t,
                ),
                None => libc::send(
                    fd,
                    bytes.as_ptr() as *const libc::c_void,
                    bytes.len(),
                    flags | libc::MSG_DONTWAIT,
                ),
            }
        };
        if n < 0 { Err(last_errno()) } else { Ok(n) }
    })?;
    Ok(Value::integer(n as i64))
}

///
/// ### UNIXSocket.new
///
/// - new(path) -> UNIXSocket
///
/// Connects to a UNIX-domain stream server. A full backlog (EAGAIN from
/// a non-blocking AF_UNIX connect) parks briefly on POLLOUT and retries.
#[monoruby_builtin]
fn unix_socket_new(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let path = unix_path_bytes(globals, lfp.arg(0))?;
    let sun = sockaddr_un_from_path(&path);
    let fd = cloexec_socket(libc::AF_UNIX, libc::SOCK_STREAM, 0, true);
    if fd < 0 {
        return Err(last_errno_err(&globals.store, "socket(2)"));
    }
    let sa_len = std::mem::size_of::<libc::sockaddr_un>() as libc::socklen_t;
    loop {
        // SAFETY: connect(2) with a valid sockaddr_un.
        let rc = unsafe { libc::connect(fd, &sun as *const libc::sockaddr_un as *const libc::sockaddr, sa_len) };
        if rc == 0 {
            break;
        }
        let errno = last_errno();
        match errno {
            libc::EINPROGRESS => {
                if let Err(e) = wait_connect_settled(vm, globals, fd, None, "connect(2)") {
                    // SAFETY: closing the fd we own.
                    unsafe { libc::close(fd) };
                    return Err(e);
                }
                break;
            }
            // Backlog full on AF_UNIX: retry after a bounded park.
            libc::EAGAIN | libc::EINTR => {
                if crate::executor::execute_gc(vm, globals).is_none() {
                    // SAFETY: closing the fd we own.
                    unsafe { libc::close(fd) };
                    return Err(vm.take_error());
                }
                if errno == libc::EAGAIN {
                    let deadline = std::time::Instant::now() + std::time::Duration::from_millis(50);
                    if let Err(e) = park_on_fd(vm, globals, fd, libc::POLLOUT, Some(deadline)) {
                        // SAFETY: closing the fd we own.
                        unsafe { libc::close(fd) };
                        return Err(e);
                    }
                }
            }
            _ => {
                // SAFETY: closing the fd we own.
                unsafe { libc::close(fd) };
                return Err(errno_err(&globals.store, errno, "connect(2)"));
            }
        }
    }
    set_blocking(fd);
    let class_id = lfp.self_val().as_class().id();
    // SAFETY: fd is a fresh, owned socket fd.
    let file = unsafe { std::fs::File::from_raw_fd(fd) };
    Ok(Value::new_socket(file, "UNIXSocket".into(), class_id))
}

///
/// ### UNIXServer.new
///
/// - new(path) -> UNIXServer
///
/// Binds and listens on a UNIX-domain stream socket. The listener fd is
/// left non-blocking for `accept`'s park-and-retry loop.
#[monoruby_builtin]
fn unix_server_new(_: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let path = unix_path_bytes(globals, lfp.arg(0))?;
    let sun = sockaddr_un_from_path(&path);
    let fd = cloexec_socket(libc::AF_UNIX, libc::SOCK_STREAM, 0, true);
    if fd < 0 {
        return Err(last_errno_err(&globals.store, "socket(2)"));
    }
    let sa_len = std::mem::size_of::<libc::sockaddr_un>() as libc::socklen_t;
    // SAFETY: bind/listen on the fresh fd we own.
    unsafe {
        if libc::bind(fd, &sun as *const libc::sockaddr_un as *const libc::sockaddr, sa_len) < 0 {
            let err = last_errno_err(&globals.store, "bind(2)");
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
    Ok(Value::new_socket(file, "UNIXServer".into(), class_id))
}

///
/// ### UNIXSocket#path
///
/// - path -> String
///
/// The bound path for a server-side socket, `""` for a client.
#[monoruby_builtin]
fn unix_path(_: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let sa = name_raw(&globals.store, lfp.self_val(), false)?;
    let path = sun_path_from_bytes(sa.as_rstring_inner().as_bytes()).unwrap_or_default();
    Ok(Value::string_from_vec(path))
}

#[cfg(test)]
mod tests {
    use crate::tests::*;

    #[test]
    fn tcp_echo_roundtrip() {
        run_test_once(
            r#"
            require "socket"
            s = TCPServer.new("127.0.0.1", 0)
            port = s.addr[1]
            t = Thread.new { c = s.accept; c.write(c.gets); c.close }
            k = TCPSocket.new("127.0.0.1", port)
            k.puts "hello"
            res = k.gets
            k.close
            t.join
            s.close
            res
            "#,
        );
    }

    #[test]
    fn tcp_open_block_and_connect_timeout() {
        run_test_once(
            r#"
            require "socket"
            s = TCPServer.new("127.0.0.1", 0)
            port = s.addr[1]
            t = Thread.new { c = s.accept; c.write("pong\n"); c.close }
            res = TCPSocket.open("127.0.0.1", port, connect_timeout: 5) { |k| k.gets }
            t.join
            s.close
            res
            "#,
        );
    }

    #[test]
    fn tcp_addr_and_peeraddr() {
        run_test_once(
            r#"
            require "socket"
            s = TCPServer.new("127.0.0.1", 0)
            port = s.addr[1]
            t = Thread.new { c = s.accept; sleep 0.2; c.close }
            k = TCPSocket.new("127.0.0.1", port)
            res = [
              s.addr.values_at(0, 2, 3),
              s.addr[1].is_a?(Integer),
              k.peeraddr.values_at(0, 2, 3),
              k.peeraddr[1] == port,
              k.addr[0],
              IPSocket.getaddress("127.0.0.1"),
            ]
            k.close
            t.join
            s.close
            res
            "#,
        );
    }

    #[test]
    fn udp_roundtrip_and_recvfrom() {
        run_test_once(
            r#"
            require "socket"
            s = UDPSocket.new
            s.bind("127.0.0.1", 0)
            port = s.addr[1]
            c = UDPSocket.new
            c.send("ping", 0, "127.0.0.1", port)
            msg, addr = s.recvfrom(10)
            res = [msg, addr[0], addr[3], addr[1].is_a?(Integer)]
            c.close
            s.close
            res
            "#,
        );
    }

    #[test]
    fn socket_generic_accept_connect() {
        run_test_once(
            r#"
            require "socket"
            srv = Socket.new(:INET, :STREAM)
            srv.setsockopt(:SOCKET, :REUSEADDR, true)
            srv.bind(Socket.pack_sockaddr_in(0, "127.0.0.1"))
            srv.listen(5)
            port = srv.local_address.ip_port
            cli = Socket.new(:INET, :STREAM)
            t = Thread.new { cli.connect(Socket.pack_sockaddr_in(port, "127.0.0.1")) }
            conn, ai = srv.accept
            t.join
            res = [conn.class.to_s, ai.class.to_s, ai.ip_address, ai.ip_port.is_a?(Integer)]
            conn.close; cli.close; srv.close
            res
            "#,
        );
    }

    #[test]
    fn unix_pair_recv_send_and_recvmsg() {
        run_test_once(
            r#"
            require "socket"
            a, b = UNIXSocket.pair
            a.send("hello", 0)
            got = b.recv(5)
            b.sendmsg("world")
            data, ai, _fl = a.recvmsg(5)
            res = [got, data, ai.class.to_s, a.class.to_s]
            a.close; b.close
            res
            "#,
        );
    }

    #[test]
    fn unix_server_client_roundtrip() {
        run_test_once(
            r#"
            require "socket"
            require "tmpdir"
            path = File.join(Dir.mktmpdir, "s.sock")
            srv = UNIXServer.new(path)
            t = Thread.new { c = srv.accept; c.write(c.gets); c.close }
            k = UNIXSocket.new(path)
            k.puts "unix!"
            res = [k.gets, srv.path]
            k.close
            t.join
            srv.close
            res.map { |s| s.sub(File.dirname(path), "") }
            "#,
        );
    }

    #[test]
    fn recv_returns_nil_at_stream_eof() {
        run_test_once(
            r#"
            require "socket"
            a, b = UNIXSocket.pair
            a.close
            res = b.recv(10)
            b.close
            res.inspect
            "#,
        );
    }

    #[test]
    fn pack_unpack_sockaddr() {
        run_test_once(
            r#"
            require "socket"
            sa = Socket.pack_sockaddr_in(8080, "127.0.0.1")
            un = Socket.pack_sockaddr_un("/tmp/x.sock")
            [
              Socket.unpack_sockaddr_in(sa),
              Socket.unpack_sockaddr_un(un),
              sa.encoding.to_s,
              begin; Socket.unpack_sockaddr_in(un); rescue ArgumentError => e; :bad_family; end,
            ]
            "#,
        );
    }

    #[test]
    fn blocked_socket_ops_report_sleep_status() {
        // The exact discipline ruby/spec's `block_caller` matcher spins
        // on: a thread blocked in a socket op must report status
        // "sleep", and Thread#kill must be able to reap it.
        run_test_once(
            r#"
            require "socket"
            require "tmpdir"
            res = []
            probe = lambda do |&blk|
              t = Thread.new(&blk)
              st = nil
              5000.times do
                st = t.status
                break if st == "sleep" || st == false || st.nil?
                Thread.pass
              end
              t.kill
              t.join
              st
            end
            s = TCPServer.new("127.0.0.1", 0)
            res << probe.call { s.accept }
            u = UDPSocket.new
            u.bind("127.0.0.1", 0)
            res << probe.call { u.recvfrom(10) }
            a, b = UNIXSocket.pair
            res << probe.call { a.recv(4) }
            res << probe.call { a.recvmsg }
            res << probe.call { Socket.tcp_server_loop("127.0.0.1", 0) { } }
            [s, u, a, b].each(&:close)
            res
            "#,
        );
    }

    #[test]
    fn tcp_connect_refused() {
        run_test_once(
            r#"
            require "socket"
            # Grab a port that is momentarily free, then connect after closing.
            s = TCPServer.new("127.0.0.1", 0)
            port = s.addr[1]
            s.close
            begin
              TCPSocket.new("127.0.0.1", port)
              "connected"
            rescue Errno::ECONNREFUSED
              "refused"
            end
            "#,
        );
    }

    #[test]
    fn accept_closed_in_another_thread() {
        run_test_once(
            r#"
            require "socket"
            s = TCPServer.new("127.0.0.1", 0)
            t = Thread.new do
              begin
                s.accept
                "accepted"
              rescue IOError => e
                e.message
              end
            end
            sleep 0.2
            s.close
            t.value
            "#,
        );
    }

    #[test]
    fn shutdown_write_gives_server_eof() {
        run_test_once(
            r#"
            require "socket"
            s = TCPServer.new("127.0.0.1", 0)
            port = s.addr[1]
            t = Thread.new { c = s.accept; got = c.read; c.write(got.upcase); c.close; got }
            k = TCPSocket.new("127.0.0.1", port)
            k.write "chunked body"
            k.shutdown(Socket::SHUT_WR)
            res = [k.read, t.value]
            k.close
            s.close
            res
            "#,
        );
    }

    #[test]
    fn socket_new_and_constants() {
        run_test_once(
            r#"
            require "socket"
            sock = Socket.new(:INET, :STREAM)
            res = [
              sock.is_a?(Socket),
              sock.is_a?(BasicSocket),
              sock.close,
              Socket::AF_INET == Socket::Constants::AF_INET,
              Socket::SOCK_STREAM.is_a?(Integer),
              Socket::SOL_SOCKET.is_a?(Integer),
              Socket::SOMAXCONN >= 1,
              TCPSocket.superclass,
              TCPServer.superclass,
              BasicSocket.superclass,
              SocketError.superclass,
            ]
            res
            "#,
        );
    }

    // getsockopt returns a plain Integer in monoruby (CRuby wraps it in
    // Socket::Option), so assert monoruby's behavior directly instead of
    // comparing with CRuby.
    #[test]
    fn set_and_getsockopt() {
        let v = run_test_no_result_check(
            r#"
            require "socket"
            s = TCPServer.new("127.0.0.1", 0)
            s.setsockopt(Socket::SOL_SOCKET, Socket::SO_KEEPALIVE, true)
            on = s.getsockopt(Socket::SOL_SOCKET, Socket::SO_KEEPALIVE)
            s.setsockopt(Socket::SOL_SOCKET, Socket::SO_KEEPALIVE, false)
            off = s.getsockopt(Socket::SOL_SOCKET, Socket::SO_KEEPALIVE)
            s.close
            [on != 0, off].inspect
            "#,
        );
        assert_eq!("[true, 0]", v.as_str());
    }

    #[test]
    fn gets_read_partial_across_packets() {
        run_test_once(
            r#"
            require "socket"
            s = TCPServer.new("127.0.0.1", 0)
            port = s.addr[1]
            t = Thread.new do
              c = s.accept
              c.write "line one\n"
              sleep 0.1
              c.write "line two\nrest"
              c.close
            end
            k = TCPSocket.new("127.0.0.1", port)
            res = [k.gets, k.gets, k.read]
            k.close
            t.join
            s.close
            res
            "#,
        );
    }

    #[test]
    fn constructor_variants_and_errors() {
        run_test_once(
            r#"
            require "socket"
            res = []
            # unknown host -> SocketError
            begin
              TCPSocket.new("no-such-host.invalid", 80)
              res << "connected"
            rescue SocketError
              res << "SocketError"
            end
            # TCPServer.new(port) form; listen; numeric addr. (Do not
            # assert s.addr[0]: with the host omitted, CRuby binds the
            # IPv6 wildcard on macOS — AF_INET6 — but AF_INET on Linux.)
            s = TCPServer.new(0)
            res << s.addr[1].is_a?(Integer)
            s2 = TCPServer.new("127.0.0.1", 0)
            res << s2.listen(5)
            res << s2.addr(:numeric).values_at(0, 2, 3)
            # Socket.new with String family/type and explicit protocol
            sock = Socket.new("INET", "STREAM", 0)
            res << sock.is_a?(Socket)
            sock.close
            # 4-arg TCPSocket.new with local bind
            port = s2.addr[1]
            t = Thread.new { c = s2.accept; c.close }
            k = TCPSocket.new("127.0.0.1", port, "127.0.0.1", 0)
            res << k.addr[2]
            k.close
            t.join
            s.close
            s2.close
            res
            "#,
        );
    }

    #[test]
    fn shutdown_symbol_and_eof_read() {
        run_test_once(
            r#"
            require "socket"
            s = TCPServer.new("127.0.0.1", 0)
            got = nil
            t = Thread.new { c = s.accept; got = c.read; c.close }
            k = TCPSocket.new("127.0.0.1", s.addr[1])
            k.shutdown(:WR)
            t.join
            k.close
            s.close
            got
            "#,
        );
    }
}
