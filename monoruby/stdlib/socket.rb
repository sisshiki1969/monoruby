# Ruby-level remainder of the socket library for monoruby.
#
# The real TCP surface — BasicSocket < IO, IPSocket, TCPSocket, TCPServer,
# Socket, Socket::Constants, SocketError — is implemented natively
# (src/builtins/socket.rs): sockets are IO objects over the socket fd, so
# the whole IO method set (read/gets/write/close/closed?/…, including the
# green-thread blocking-IO emulation) applies to them by inheritance, and
# accept/connect park the calling green thread on the scheduler's fd
# poller. This file adds only pure-Ruby bookkeeping and the stubs for the
# protocols that remain unsupported (UNIX domain, UDP).

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
end

class Socket
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

  def self.gethostname
    require "etc" rescue nil
    (Etc.uname[:nodename] rescue nil) || "localhost"
  end

  def self.getaddrinfo(*_)
    []
  end

  def self.ip_address_list
    []
  end
end

class UDPSocket < IPSocket
  # UDP is not implemented. Fail loudly at instantiation (see UNIXSocket
  # below): a silent do-nothing socket lets code run on until it dies far
  # from the cause.
  def initialize(*)
    raise SocketError, "UDP sockets are not supported in monoruby"
  end
end

class UNIXSocket < BasicSocket
  # Real UNIX-domain sockets are unsupported, but the path argument is
  # validated like CRuby's (CVE-2018-8779): a NUL byte must raise
  # ArgumentError *before* any bind/connect would happen. Valid paths
  # fail with an explicit unsupported-feature error instead of a
  # misleading NoMethodError.
  def self.open(path, *rest, &blk)
    new(path, *rest, &blk)
  end

  def initialize(path, *)
    path = path.to_path if path.respond_to?(:to_path)
    path = path.to_str  if path.respond_to?(:to_str)
    unless path.is_a?(String)
      raise TypeError, "no implicit conversion of #{path.class} into String"
    end
    if path.include?("\0")
      raise ArgumentError, "path name contains null byte"
    end
    raise SocketError, "UNIX domain sockets are not supported in monoruby"
  end
end
class UNIXServer < UNIXSocket; end

class Addrinfo
  def self.tcp(host, port); new; end
  def self.udp(host, port); new; end
  def ip_address; "0.0.0.0"; end
  def ip_port; 0; end
end
