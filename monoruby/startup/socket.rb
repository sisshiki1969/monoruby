# Minimal socket stub for monoruby
# Provides just enough class/constant definitions for Bundler and gems
# that reference socket types without actually using network I/O.

class BasicSocket < IO
end

class IPSocket < BasicSocket
end

class TCPSocket < IPSocket
  def initialize(host, port, *args)
    raise Errno::ECONNREFUSED, "monoruby does not support socket I/O"
  end
end

class TCPServer < TCPSocket
  def initialize(host = nil, port = nil)
    raise Errno::ECONNREFUSED, "monoruby does not support socket I/O"
  end
end

class UDPSocket < IPSocket
  def initialize(family = nil)
    raise Errno::ECONNREFUSED, "monoruby does not support socket I/O"
  end
end

class UNIXSocket < BasicSocket
  def initialize(path)
    raise Errno::ECONNREFUSED, "monoruby does not support socket I/O"
  end
end

class UNIXServer < UNIXSocket
end

class Socket < BasicSocket
  # Address families
  AF_UNSPEC = 0
  AF_UNIX   = 1
  AF_INET   = 2
  AF_INET6  = 10

  PF_UNSPEC = AF_UNSPEC
  PF_UNIX   = AF_UNIX
  PF_INET   = AF_INET
  PF_INET6  = AF_INET6

  # Socket types
  SOCK_STREAM = 1
  SOCK_DGRAM  = 2
  SOCK_RAW    = 3

  # IP protocols
  IPPROTO_TCP = 6
  IPPROTO_UDP = 17

  # Socket options
  SOL_SOCKET    = 1
  SO_REUSEADDR  = 2
  SO_KEEPALIVE  = 9

  def initialize(*args)
    raise Errno::ECONNREFUSED, "monoruby does not support socket I/O"
  end

  def self.getaddrinfo(host, port, *args)
    []
  end

  def self.gethostname
    "localhost"
  end

  def self.pack_sockaddr_in(port, host)
    "\x02\x00".b + [port.to_i].pack("n") + [0].pack("N") + ("\x00" * 8).b
  end

  def self.unpack_sockaddr_in(sockaddr)
    [0, "0.0.0.0"]
  end

  def self.ip_address_list
    []
  end
end

class Addrinfo
  def initialize(*args)
  end

  def self.getaddrinfo(*args)
    []
  end

  def self.tcp(host, port)
    new
  end

  def self.udp(host, port)
    new
  end

  def ip?
    false
  end

  def unix?
    false
  end
end

module Socket::Constants
  AF_UNSPEC   = Socket::AF_UNSPEC
  AF_UNIX     = Socket::AF_UNIX
  AF_INET     = Socket::AF_INET
  AF_INET6    = Socket::AF_INET6
  PF_UNSPEC   = Socket::PF_UNSPEC
  PF_UNIX     = Socket::PF_UNIX
  PF_INET     = Socket::PF_INET
  PF_INET6    = Socket::PF_INET6
  SOCK_STREAM = Socket::SOCK_STREAM
  SOCK_DGRAM  = Socket::SOCK_DGRAM
  SOCK_RAW    = Socket::SOCK_RAW
  IPPROTO_TCP = Socket::IPPROTO_TCP
  IPPROTO_UDP = Socket::IPPROTO_UDP
  SOL_SOCKET  = Socket::SOL_SOCKET
  SO_REUSEADDR = Socket::SO_REUSEADDR
  SO_KEEPALIVE = Socket::SO_KEEPALIVE
end
