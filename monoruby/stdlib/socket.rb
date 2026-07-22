# Ruby-level remainder of the socket library for monoruby.
#
# The low-level surface — BasicSocket < IO, IPSocket, TCPSocket,
# TCPServer, UDPSocket, UNIXSocket, UNIXServer, Socket — is implemented
# natively (src/builtins/socket.rs): sockets are IO objects over the
# socket fd, and every potentially blocking operation (accept/connect/
# recv/send/recvfrom/recvmsg/sendmsg) parks the calling green thread on
# the scheduler's fd poller, so `Thread#status` reports "sleep" while
# blocked. This file wraps the raw natives (`__accept_raw`,
# `__recvfrom_raw`, …) into the CRuby-shaped results (Addrinfo, address
# arrays) and adds the pure-Ruby conveniences (server-loop helpers).

class BasicSocket
  # monoruby always returns numeric addresses (as if
  # do_not_reverse_lookup were true); the accessors exist for
  # compatibility.
  @do_not_reverse_lookup = true
  def self.do_not_reverse_lookup
    @do_not_reverse_lookup.nil? ? true : @do_not_reverse_lookup
  end

  def self.do_not_reverse_lookup=(v)
    @do_not_reverse_lookup = v
  end

  def do_not_reverse_lookup
    @do_not_reverse_lookup.nil? ? BasicSocket.do_not_reverse_lookup : @do_not_reverse_lookup
  end

  def do_not_reverse_lookup=(v)
    @do_not_reverse_lookup = v
  end

  def local_address
    Addrinfo.new(__sockname_raw)
  end

  def remote_address
    Addrinfo.new(__peername_raw)
  end

  def getsockname
    __sockname_raw
  end

  def getpeername
    __peername_raw
  end

  # The local address rewritten to something a client can connect to:
  # a wildcard IP (0.0.0.0) becomes the loopback address.
  def connect_address
    ai = local_address
    if ai.ipv4? && ai.ip_address == "0.0.0.0"
      Addrinfo.new(Socket.pack_sockaddr_in(ai.ip_port, "127.0.0.1"))
    else
      ai
    end
  end

  # send(mesg, flags, dest_sockaddr = nil) -> Integer
  # Wraps the native primitive to accept an Addrinfo destination.
  def send(mesg, flags, dest = nil)
    dest = dest.to_sockaddr if dest.respond_to?(:to_sockaddr)
    __send_raw(mesg, flags, dest)
  end

  # recvmsg(maxdatalen = nil, flags = 0, maxcontrollen = nil, opts = {})
  #   -> [data, sender_addrinfo, rflags] | nil at stream EOF
  # Ancillary data is not supported (no controls in the result).
  def recvmsg(dlen = nil, flags = 0, _clen = nil, _opts = {})
    raw = __recvmsg_raw(dlen || 65536, flags, true)
    return nil if raw.nil?
    data, sa, rflags = raw
    [data, Addrinfo.new(sa, nil, __socktype), rflags]
  end

  def recvmsg_nonblock(dlen = nil, flags = 0, _clen = nil, _opts = {}, exception: true)
    raw = begin
      __recvmsg_raw(dlen || 65536, flags, false)
    rescue IO::WaitReadable
      raise if exception
      return :wait_readable
    end
    return nil if raw.nil?
    data, sa, rflags = raw
    [data, Addrinfo.new(sa, nil, __socktype), rflags]
  end

  # sendmsg(mesg, flags = 0, dest_sockaddr = nil, *controls) -> Integer
  def sendmsg(mesg, flags = 0, dest = nil, *_controls)
    dest = dest.to_sockaddr if dest.respond_to?(:to_sockaddr)
    __sendmsg_raw(mesg, flags, dest, true)
  end

  def sendmsg_nonblock(mesg, flags = 0, dest = nil, *_controls, exception: true)
    dest = dest.to_sockaddr if dest.respond_to?(:to_sockaddr)
    begin
      __sendmsg_raw(mesg, flags, dest, false)
    rescue IO::WaitWritable
      raise if exception
      :wait_writable
    end
  end

  # The socket's own SO_TYPE, used to tag sender Addrinfos.
  def __socktype
    @__socktype ||= getsockopt(Socket::SOL_SOCKET, Socket::SO_TYPE)
  end
  private :__socktype
end

class IPSocket
  # recvfrom(maxlen, flags = 0) -> [data, ["AF_INET", port, host, host]]
  def recvfrom(maxlen, flags = 0)
    raw = __recvfrom_raw(maxlen, flags, true)
    return nil if raw.nil?
    data, sa = raw
    [data, Addrinfo.new(sa).__ip_addr_array]
  end
end

class Socket
  # recvfrom(maxlen, flags = 0) -> [data, Addrinfo] | nil at stream EOF
  def recvfrom(maxlen, flags = 0)
    raw = __recvfrom_raw(maxlen, flags, true)
    return nil if raw.nil?
    data, sa = raw
    [data, Addrinfo.new(sa, nil, __socktype)]
  end

  # accept -> [Socket, Addrinfo]
  def accept
    sock, sa = __accept_raw(Socket)
    [sock, Addrinfo.new(sa)]
  end

  # sysaccept -> [Integer, Addrinfo]
  def sysaccept
    fd, sa = __accept_raw(nil)
    [fd, Addrinfo.new(sa)]
  end

  # The high-level connect helper the net/* libraries use. Delegates to
  # the native TCPSocket; extra keywords accepted for compatibility
  # (resolv_timeout / fast_fallback are meaningless for a blocking
  # single-resolver connect).
  def self.tcp(host, port, local_host = nil, local_port = nil,
               connect_timeout: nil, resolv_timeout: nil,
               open_timeout: nil, fast_fallback: nil)
    timeout = connect_timeout || open_timeout
    sock = TCPSocket.new(host, port, local_host, local_port,
                         connect_timeout: timeout)
    if block_given?
      begin
        yield sock
      ensure
        sock.close unless sock.closed?
      end
    else
      sock
    end
  end

  # One bound+listening IPv4 stream Socket (CRuby returns one socket per
  # resolved address; monoruby resolves IPv4 only).
  def self.tcp_server_sockets(host = nil, port)
    sock = Socket.new(:INET, :STREAM)
    sock.setsockopt(:SOCKET, :REUSEADDR, true)
    sock.bind(Socket.pack_sockaddr_in(port || 0, host))
    sock.listen(Socket::SOMAXCONN)
    sockets = [sock]
    if block_given?
      begin
        yield sockets
      ensure
        sockets.each { |s| s.close unless s.closed? }
      end
    else
      sockets
    end
  end

  # Accept connections on the listening sockets forever, yielding each
  # connection as [Socket, Addrinfo]. Blocks (parks) in IO.select
  # between connections.
  #
  # NOTE: the *_loop helpers below inline this instead of forwarding
  # their block with `&b`: a directly-yielded block keeps `break`
  # returning from the helper the user called, while a re-captured
  # block would turn `break` into a proc-closure escape.
  def self.accept_loop(*sockets)
    sockets.flatten!
    loop do
      readable, = IO.select(sockets)
      readable.each do |sock|
        conn, addr = sock.accept
        yield conn, addr
      end
    end
  end

  def self.tcp_server_loop(host = nil, port)
    tcp_server_sockets(host, port) do |sockets|
      loop do
        readable, = IO.select(sockets)
        readable.each do |sock|
          conn, addr = sock.accept
          yield conn, addr
        end
      end
    end
  end

  # One bound IPv4 datagram Socket.
  def self.udp_server_sockets(host = nil, port)
    sock = Socket.new(:INET, :DGRAM)
    sock.setsockopt(:SOCKET, :REUSEADDR, true)
    sock.bind(Socket.pack_sockaddr_in(port || 0, host))
    sockets = [sock]
    if block_given?
      begin
        yield sockets
      ensure
        sockets.each { |s| s.close unless s.closed? }
      end
    else
      sockets
    end
  end

  # Receive datagrams forever, yielding [message, Socket::UDPSource].
  def self.udp_server_loop_on(sockets)
    loop do
      readable, = IO.select(sockets)
      readable.each do |sock|
        msg, sender = sock.recvfrom(65536)
        yield msg, UDPSource.new(sender, sock)
      end
    end
  end

  def self.udp_server_loop(host = nil, port)
    udp_server_sockets(host, port) do |sockets|
      loop do
        readable, = IO.select(sockets)
        readable.each do |sock|
          msg, sender = sock.recvfrom(65536)
          yield msg, UDPSource.new(sender, sock)
        end
      end
    end
  end

  # Connect a UNIX-domain stream Socket to `path`.
  def self.unix(path)
    sock = Socket.new(:UNIX, :STREAM)
    sock.connect(Socket.pack_sockaddr_un(path))
    if block_given?
      begin
        yield sock
      ensure
        sock.close unless sock.closed?
      end
    else
      sock
    end
  end

  def self.unix_server_socket(path)
    sock = Socket.new(:UNIX, :STREAM)
    sock.bind(Socket.pack_sockaddr_un(path))
    sock.listen(Socket::SOMAXCONN)
    if block_given?
      begin
        yield sock
      ensure
        sock.close unless sock.closed?
        begin
          File.unlink(path)
        rescue StandardError
        end
      end
    else
      sock
    end
  end

  def self.unix_server_loop(path)
    unix_server_socket(path) do |sock|
      loop do
        IO.select([sock])
        conn, addr = sock.accept
        yield conn, addr
      end
    end
  end

  # The reply handle yielded by udp_server_loop.
  class UDPSource
    attr_reader :remote_address

    def initialize(remote_address, socket)
      @remote_address = remote_address
      @socket = socket
    end

    def reply(msg)
      @socket.send(msg, 0, @remote_address.to_sockaddr)
    end

    def inspect
      "#<Socket::UDPSource: #{@remote_address.inspect}>"
    end
  end

  def self.gethostname
    begin
      require "etc"
    rescue StandardError
    end
    begin
      Etc.uname[:nodename]
    rescue StandardError
      nil
    end || "localhost"
  end

  # Minimal getaddrinfo: numeric IPv4 resolution only (monoruby always
  # behaves as do_not_reverse_lookup = true, so the canonical-name slot
  # carries the numeric address, as CRuby does in that mode).
  def self.getaddrinfo(host, service, _family = nil, socktype = nil,
                       protocol = nil, _flags = nil, _reverse_lookup = nil)
    port = case service
           when nil then 0
           when Integer then service
           else service.to_s.to_i
           end
    ip = IPSocket.getaddress(host.nil? || host == "" ? "127.0.0.1" : host.to_s)
    st = socktype.nil? || socktype == 0 ? Socket::SOCK_STREAM : socktype
    [["AF_INET", port, ip, ip, Socket::PF_INET, st, protocol || 0]]
  end

  def self.ip_address_list
    []
  end
end

class UDPSocket < IPSocket
  # send(mesg, flags [, host, port] / [, sockaddr_to]) -> Integer
  # Wraps the native primitive to accept an Addrinfo destination.
  def send(mesg, flags, *rest)
    if rest.length == 1 && rest[0].respond_to?(:to_sockaddr)
      rest = [rest[0].to_sockaddr]
    end
    __udp_send_raw(mesg, flags, *rest)
  end

  # UDPSocket#recvfrom_nonblock(maxlen, flags = 0, outbuf = nil, exception: true)
  #   -> [data, ["AF_INET", sender_port, sender_host, sender_host]]
  def recvfrom_nonblock(maxlen, flags = 0, _outbuf = nil, exception: true)
    begin
      data, sa = __recvfrom_raw(maxlen, flags, false)
    rescue IO::WaitReadable
      raise if exception
      return :wait_readable
    end
    [data, Addrinfo.new(sa).__ip_addr_array]
  end
end

class UNIXSocket < BasicSocket
  def self.open(path, &blk)
    sock = new(path)
    if blk
      begin
        blk.call(sock)
      ensure
        sock.close unless sock.closed?
      end
    else
      sock
    end
  end

  # recvfrom(maxlen, flags = 0) -> [data, ["AF_UNIX", path]]
  def recvfrom(maxlen, flags = 0)
    raw = __recvfrom_raw(maxlen, flags, true)
    return nil if raw.nil?
    data, sa = raw
    path = if sa.empty?
      ""
    else
      begin
        Socket.unpack_sockaddr_un(sa)
      rescue StandardError
        ""
      end
    end
    [data, ["AF_UNIX", path]]
  end

  def addr
    ["AF_UNIX", path]
  end

  def peeraddr
    sa = __peername_raw
    path = begin
      Socket.unpack_sockaddr_un(sa)
    rescue StandardError
      ""
    end
    ["AF_UNIX", path]
  end
end

class UNIXServer < UNIXSocket
  def self.open(path, &blk)
    sock = new(path)
    if blk
      begin
        blk.call(sock)
      ensure
        sock.close unless sock.closed?
      end
    else
      sock
    end
  end

  # accept -> UNIXSocket
  def accept
    sock, _sa = __accept_raw(UNIXSocket)
    sock
  end

  # sysaccept -> Integer
  def sysaccept
    fd, _sa = __accept_raw(nil)
    fd
  end
end

# A minimal Addrinfo over a packed sockaddr (AF_INET / AF_UNIX).
class Addrinfo
  attr_reader :socktype, :protocol

  def initialize(sockaddr, _family = nil, socktype = nil, protocol = nil)
    if sockaddr.is_a?(Array)
      # ["AF_INET", port, host, numeric_host] / ["AF_UNIX", path]
      case sockaddr[0]
      when "AF_INET"
        sockaddr = Socket.pack_sockaddr_in(sockaddr[1], sockaddr[3] || sockaddr[2])
      when "AF_UNIX"
        sockaddr = Socket.pack_sockaddr_un(sockaddr[1])
      else
        raise SocketError, "unknown address family: #{sockaddr[0]}"
      end
    end
    @sockaddr = sockaddr.to_s.b
    @socktype = socktype || 0
    @protocol = protocol || 0
  end

  def self.ip(host)
    new(Socket.pack_sockaddr_in(0, host))
  end

  def self.tcp(host, port)
    new(Socket.pack_sockaddr_in(port, host))
  end

  def self.udp(host, port)
    new(Socket.pack_sockaddr_in(port, host))
  end

  def self.unix(path)
    new(Socket.pack_sockaddr_un(path))
  end

  def to_sockaddr
    @sockaddr
  end
  alias to_s to_sockaddr

  def afamily
    # sa_family's offset/width is platform-dependent (Linux: u16 at 0;
    # macOS/BSD: u8 after sa_len) — the native helper reads it through
    # the real struct sockaddr layout.
    Socket.__sockaddr_family(@sockaddr)
  end
  alias pfamily afamily

  def ip?
    afamily == Socket::AF_INET || afamily == Socket::AF_INET6
  end

  def ipv4?
    afamily == Socket::AF_INET
  end

  def ipv6?
    afamily == Socket::AF_INET6
  end

  def unix?
    afamily == Socket::AF_UNIX
  end

  def ip_address
    raise SocketError, "need IPv4 or IPv6 address" unless ip?
    Socket.unpack_sockaddr_in(@sockaddr)[1]
  end

  def ip_port
    raise SocketError, "need IPv4 or IPv6 address" unless ip?
    Socket.unpack_sockaddr_in(@sockaddr)[0]
  end

  def ip_unpack
    raise SocketError, "need IPv4 or IPv6 address" unless ip?
    port, addr = Socket.unpack_sockaddr_in(@sockaddr)
    [addr, port]
  end

  def unix_path
    raise SocketError, "need AF_UNIX address" unless unix?
    Socket.unpack_sockaddr_un(@sockaddr)
  end

  # Internal: IPSocket#recvfrom-shaped address array.
  def __ip_addr_array
    port, addr = Socket.unpack_sockaddr_in(@sockaddr)
    ["AF_INET", port, addr, addr]
  end

  def inspect
    if unix?
      "#<Addrinfo: #{unix_path} SOCK_STREAM>"
    elsif ip?
      "#<Addrinfo: #{ip_address}:#{ip_port}>"
    else
      "#<Addrinfo: (unknown)>"
    end
  end
end
