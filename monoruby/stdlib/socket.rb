# Minimal Socket / BasicSocket / IPSocket stub for monoruby.
#
# The real socket C extension is unsupported. ActiveSupport loads socket.rb
# via `core_ext/object/json.rb` (for IPAddr → JSON serialization) and
# ipaddr.rb autoloads it too, but none of that actually requires a working
# socket during class loading. Provide empty classes so the constant
# references resolve.

class BasicSocket
  def self.do_not_reverse_lookup; true; end
  def self.do_not_reverse_lookup=(v); v; end
end

class Socket < BasicSocket
  AF_INET = 2
  AF_INET6 = 10
  AF_UNIX = 1
  SOCK_STREAM = 1
  SOCK_DGRAM = 2
  INADDR_ANY = "0.0.0.0"

  class Constants; end

  def self.gethostname
    "localhost"
  end

  def self.getaddrinfo(*_); []; end
  def self.ip_address_list; []; end
end

class IPSocket < BasicSocket
  def self.getaddress(host); host; end
end

class TCPSocket < IPSocket; end
class TCPServer < TCPSocket; end
class UDPSocket < IPSocket; end
class UNIXSocket < BasicSocket
  # Real UNIX-domain sockets are unsupported, but the path argument is
  # validated like CRuby's (CVE-2018-8779): a NUL byte must raise
  # ArgumentError *before* any bind/connect would happen. Valid paths
  # fail with an explicit unsupported-feature error instead of the
  # previous misleading NoMethodError.
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

class SocketError < StandardError; end

class Addrinfo
  def self.tcp(host, port); new; end
  def self.udp(host, port); new; end
  def ip_address; "0.0.0.0"; end
  def ip_port; 0; end
end
