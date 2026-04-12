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
class UNIXSocket < BasicSocket; end
class UNIXServer < UNIXSocket; end

class SocketError < StandardError; end

class Addrinfo
  def self.tcp(host, port); new; end
  def self.udp(host, port); new; end
  def ip_address; "0.0.0.0"; end
  def ip_port; 0; end
end
