module File::Constants
  FNM_SYSCASE = 0
  FNM_NOESCAPE = 1
  FNM_PATHNAME = 2
  FNM_DOTMATCH = 4
  FNM_CASEFOLD = 8
  FNM_EXTGLOB = 16
  # Open(2) flags and flock(2) operations, mirrored on File itself
  # below via `include File::Constants` in CRuby; monoruby historically
  # defined them directly on File, so keep both in sync. The values
  # come from the libc-backed IO:: constants so they match the running
  # platform's kernel bits — Linux and Darwin disagree on almost every
  # one (e.g. O_APPEND is 0o2000 on Linux but 0x8 on Darwin), and
  # comparisons against fcntl(F_GETFL) results depend on this.
  RDONLY   = IO::RDONLY
  WRONLY   = IO::WRONLY
  RDWR     = IO::RDWR
  APPEND   = IO::APPEND
  CREAT    = IO::CREAT
  EXCL     = IO::EXCL
  TRUNC    = IO::TRUNC
  NOCTTY   = IO::NOCTTY
  NONBLOCK = IO::NONBLOCK
  DSYNC    = IO::DSYNC
  SYNC     = IO::SYNC
  RSYNC    = IO::RSYNC if defined?(IO::RSYNC)
  DIRECT   = IO::DIRECT if defined?(IO::DIRECT)
  NOFOLLOW = IO::NOFOLLOW
  # No-op outside Windows; defined for source compatibility (CRuby).
  SHARE_DELETE = 0
  BINARY   = 0
  LOCK_SH  = 1
  LOCK_EX  = 2
  LOCK_UN  = 8
  LOCK_NB  = 4
  NULL = "/dev/null"
end

class File
  include File::Constants
  FNM_SYSCASE = 0
  FNM_NOESCAPE = 1
  FNM_PATHNAME = 2
  FNM_DOTMATCH = 4
  FNM_CASEFOLD = 8
  FNM_EXTGLOB = 16
  NULL = "/dev/null"
  BINARY = 0

  RDONLY   = IO::RDONLY
  WRONLY   = IO::WRONLY
  RDWR     = IO::RDWR
  APPEND   = IO::APPEND
  CREAT    = IO::CREAT
  EXCL     = IO::EXCL
  TRUNC    = IO::TRUNC
  NOCTTY   = IO::NOCTTY
  NONBLOCK = IO::NONBLOCK
  DSYNC    = IO::DSYNC
  SYNC     = IO::SYNC
  RSYNC    = IO::RSYNC if defined?(IO::RSYNC)
  DIRECT   = IO::DIRECT if defined?(IO::DIRECT)
  NOFOLLOW = IO::NOFOLLOW
  SHARE_DELETE = 0
  LOCK_SH  = 1
  LOCK_EX  = 2
  LOCK_UN  = 8
  LOCK_NB  = 4

  Separator = "/"
  SEPARATOR = "/"
  ALT_SEPARATOR = nil
  PATH_SEPARATOR = ":"
end

class File
  # Instance timestamp readers (core/file/{atime,mtime,ctime,birthtime}_spec.rb).
  # monoruby only defined the File.<name>(path) class methods; delegate the
  # instance form through the receiver's #path.
  def atime;     File.atime(path);     end
  def mtime;     File.mtime(path);     end
  def ctime;     File.ctime(path);     end
  def birthtime; File.birthtime(path); end

  # Instance chmod/chown (core/file/{chmod,chown}_spec.rb): only the class
  # forms existed. Delegate through #path and return 0, as CRuby's instance
  # forms do (the class forms return the number of files affected).
  def chmod(mode);         File.chmod(mode, path);          0; end
  def chown(owner, group); File.chown(owner, group, path);  0; end

  # Purely lexical `dirname` (core/file/dirname_spec.rb). CRuby does *not*
  # resolve `.`/`..` or collapse interior slashes; it only strips the last
  # `/component` (and the slashes immediately before it), `level` times.
  def self.dirname(path, level = 1)
    path = path.to_path if !path.is_a?(String) && path.respond_to?(:to_path)
    path = path.to_str  if !path.is_a?(String) && path.respond_to?(:to_str)
    raise TypeError, "no implicit conversion of #{path.class} into String" unless path.is_a?(String)
    unless level.is_a?(Integer)
      unless level.respond_to?(:to_int)
        raise TypeError, "no implicit conversion of #{level.class} into Integer"
      end
      level = level.to_int
    end
    raise ArgumentError, "negative level: #{level}" if level < 0
    result = path
    level.times do
      prev = result
      result = _dirname_once(result)
      break if result == prev
    end
    result
  end

  def self._dirname_once(path)
    return "/" if path =~ %r{\A/+\z}   # all slashes (incl. "/")
    s = path.sub(%r{/+\z}, "")         # drop trailing slashes
    return "." if s.empty?             # empty path
    idx = s.rindex("/")
    return "." if idx.nil?             # no directory part
    prefix = s[0...idx].sub(%r{/+\z}, "")
    return "/" if prefix.empty?        # empty prefix only for absolute paths
    # Collapse a run of leading slashes to one ("/////foo/bar" → "/foo").
    prefix.sub(%r{\A/+}, "/")
  end
  private_class_method :_dirname_once

  def self.zero?(path)
    # A missing file is not an error here (returns false), but a non-path
    # argument (nil/true/Integer/...) must still raise the TypeError that
    # `File.size` produces — so rescue only filesystem errors, not everything.
    begin
      s = File.size(path)
    rescue SystemCallError
      return false
    end
    s == 0
  end

  # A true alias (same method entry), so `File.method(:empty?) ==
  # File.method(:zero?)` holds like in CRuby.
  class << self
    alias_method :empty?, :zero?
  end

  def self.readable_real?(path)
    File.readable?(path)
  end

  def self.writable_real?(path)
    File.writable?(path)
  end

  def self.executable_real?(path)
    File.executable?(path)
  end
end
