//! The socket library is CRuby's `ext/socket` built on the C API layer:
//! these pin what goes through the layer's `struct rb_io` (descriptor
//! hand-over, FMODE bits, buffered input), its waits and IO.new on a
//! subclass, against the host CRuby.

extern crate monoruby;
use monoruby::tests::*;

/// A TCP pair: buffered input seen by `recv` / `read_nonblock`, mode bits
/// `close_read` / `close_write` and `do_not_reverse_lookup` change.
#[test]
fn socket_tcp_stream() {
    run_test_once(
        r#"
        require "socket"
        res = []
        srv = TCPServer.new("127.0.0.1", 0)
        port = srv.addr[1]
        c = TCPSocket.new("127.0.0.1", port)
        s = srv.accept
        res << [s.class, c.class, s.sync, c.binmode?, s.local_address.ip_port == port]
        c.write("ab\ncd")
        res << s.gets
        res << (s.recv(10) rescue [$!.class, $!.message])
        res << s.read_nonblock(10)
        res << s.read_nonblock(10, exception: false)
        buf = +"zz"
        c.write("xyz"); IO.select([s])
        res << s.read_nonblock(10, buf) << buf
        res << [s.do_not_reverse_lookup, (s.do_not_reverse_lookup = false), s.do_not_reverse_lookup]
        c.close_write
        res << s.read
        res << (c.write("x") rescue $!.class)
        s.close_read
        res << [s.closed?, (s.read rescue $!.class)]
        s.close; c.close
        res << [s.closed?, (s.recv(1) rescue [$!.class, $!.message])]
        srv.close
        res
        "#,
    );
}

/// `IO#timeout` bounds the extension's waits; the non-blocking forms.
#[test]
fn socket_timeout_and_nonblock() {
    run_test_once(
        r#"
        require "socket"
        res = []
        srv = TCPServer.new("127.0.0.1", 0)
        srv.timeout = 0
        res << srv.timeout
        res << (srv.accept rescue $!.class)
        srv.timeout = nil
        res << srv.timeout
        res << (srv.accept_nonblock rescue $!.class)
        res << srv.accept_nonblock(exception: false)
        res << ((srv.timeout = -1) rescue $!.class)
        srv.close
        u1 = UDPSocket.new; u1.bind("127.0.0.1", 0)
        u2 = UDPSocket.new
        u2.send("hi", 0, "127.0.0.1", u1.addr[1])
        res << u1.recvfrom(10)[0]
        res << (u1.recvfrom_nonblock(10) rescue $!.class)
        u1.close; u2.close
        res
        "#,
    );
}

/// A descriptor passed over a UNIX socket becomes an IO (`rb_io_fdopen`);
/// address helpers; `IO.new` on a socket class runs its `initialize`.
#[test]
fn socket_unix_and_addrinfo() {
    run_test_once(
        r#"
        require "socket"
        res = []
        a, b = UNIXSocket.pair
        r, w = IO.pipe
        a.send_io(r)
        io = b.recv_io
        w.write("via fd"); w.close
        res << [io.class, io.read]
        a.send("dg", 0)
        res << b.recv(10)
        a.close; b.close
        res << Addrinfo.tcp("127.0.0.1", 80).inspect
        res << Socket.getaddrinfo("127.0.0.1", 80, :INET, :STREAM).first[0, 4]
        s = Socket.new(:INET, :STREAM)
        res << (s.getsockopt(:SOCKET, :TYPE).int == Socket::SOCK_STREAM)
        s.close
        res << (TCPSocket.new rescue $!.class)
        res
        "#,
    );
}
