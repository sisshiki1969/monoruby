# Minimal Etc stub for monoruby.
#
# Etc is a C extension that exposes passwd/group info and machine
# metadata. concurrent-ruby only reaches for `Etc.nprocessors`, and that
# is the only surface area ActiveRecord indirectly needs.

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

  def self.getpwuid(_uid = nil); nil; end
  def self.getpwnam(_name); nil; end
  def self.getgrgid(_gid = nil); nil; end
  def self.getgrnam(_name); nil; end
  def self.passwd; nil; end
  def self.group; nil; end
end
