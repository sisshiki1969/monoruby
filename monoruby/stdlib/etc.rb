# Etc for monoruby: passwd/group lookups are backed by hidden Rust
# builtins on Process (`Process.__getpwnam` etc. — libc getpwnam(3)
# family), so NSS-backed users resolve the same way CRuby's C extension
# does. The machine-metadata helpers remain simple stubs.

module Etc
  VERSION = "1.4.3"

  # Hard-coded to 1 because monoruby is single-threaded. concurrent-ruby
  # uses this to size thread pools; it's fine to under-report.
  def self.nprocessors
    1
  end

  def self.sysconf(_name)
    nil
  end

  def self.sysconfdir
    "/etc"
  end

  def self.systmpdir
    "/tmp"
  end

  def self.uname
    { sysname: "Linux", nodename: "monoruby", release: "", version: "", machine: "x86_64" }
  end

  class Passwd
    attr_accessor :name, :passwd, :uid, :gid, :gecos, :dir, :shell
  end

  class Group
    attr_accessor :name, :passwd, :gid, :mem
  end

  def self.__passwd(h)
    return nil if h.nil?
    pw = Passwd.new
    pw.name = h[:name]
    pw.passwd = h[:passwd]
    pw.uid = h[:uid]
    pw.gid = h[:gid]
    pw.gecos = h[:gecos]
    pw.dir = h[:dir]
    pw.shell = h[:shell]
    pw
  end
  private_class_method :__passwd

  def self.__group(h)
    return nil if h.nil?
    gr = Group.new
    gr.name = h[:name]
    gr.passwd = h[:passwd]
    gr.gid = h[:gid]
    gr.mem = h[:mem]
    gr
  end
  private_class_method :__group

  def self.getpwuid(uid = nil)
    uid = Process.uid if uid.nil?
    pw = __passwd(Process.__getpwuid(uid))
    raise ArgumentError, "can't find user for #{uid}" if pw.nil?
    pw
  end

  def self.getpwnam(name)
    pw = __passwd(Process.__getpwnam(name))
    raise ArgumentError, "can't find user for #{name}" if pw.nil?
    pw
  end

  def self.getgrgid(gid = nil)
    gid = Process.gid if gid.nil?
    gr = __group(Process.__getgrgid(gid))
    raise ArgumentError, "can't find group for #{gid}" if gr.nil?
    gr
  end

  def self.getgrnam(name)
    gr = __group(Process.__getgrnam(name))
    raise ArgumentError, "can't find group for #{name}" if gr.nil?
    gr
  end

  def self.passwd
    getpwuid
  end

  def self.group
    getgrgid
  end
end
