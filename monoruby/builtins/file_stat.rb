# monoruby-hidden-ivars: true

class File
  # File::Stat — the fields (dev, ino, mode, nlink, uid, gid, rdev,
  # size, blksize, blocks, atime, mtime, ctime) are populated from a
  # real stat(2)/lstat(2) by the Rust constructor (`File.stat`,
  # `File.lstat`, `File::Stat.new`). The accessors and mode-bit
  # predicates below read those fields. They are hidden instance
  # variables (doc/hidden_ivars.md), `/__dev` and so on: CRuby keeps a
  # `struct stat` behind the object.
  class Stat
    # A C struct in CRuby, with no `_dump_data`: `Marshal.dump` refuses it.
    @__no_dump_data = true
    include Comparable

    def dev ; (@__dev || __uninitialized) ; end
    def ino ; (@__ino || __uninitialized) ; end
    def mode ; (@__mode || __uninitialized) ; end
    def nlink ; (@__nlink || __uninitialized) ; end
    def uid ; (@__uid || __uninitialized) ; end
    def gid ; (@__gid || __uninitialized) ; end
    def rdev ; (@__rdev || __uninitialized) ; end
    def size ; (@__size || __uninitialized) ; end
    def blksize ; (@__blksize || __uninitialized) ; end
    def blocks ; (@__blocks || __uninitialized) ; end
    def atime ; (@__atime || __uninitialized) ; end
    def mtime ; (@__mtime || __uninitialized) ; end
    def ctime ; (@__ctime || __uninitialized) ; end

    S_IFMT   = 0o170000
    S_IFSOCK = 0o140000
    S_IFLNK  = 0o120000
    S_IFREG  = 0o100000
    S_IFBLK  = 0o060000
    S_IFDIR  = 0o040000
    S_IFCHR  = 0o020000
    S_IFIFO  = 0o010000

    def directory? ; ((@__mode || __uninitialized) & S_IFMT) == S_IFDIR  ; end
    def file?      ; ((@__mode || __uninitialized) & S_IFMT) == S_IFREG  ; end
    def symlink?   ; ((@__mode || __uninitialized) & S_IFMT) == S_IFLNK  ; end
    def blockdev?  ; ((@__mode || __uninitialized) & S_IFMT) == S_IFBLK  ; end
    def chardev?   ; ((@__mode || __uninitialized) & S_IFMT) == S_IFCHR  ; end
    def pipe?      ; ((@__mode || __uninitialized) & S_IFMT) == S_IFIFO  ; end
    def socket?    ; ((@__mode || __uninitialized) & S_IFMT) == S_IFSOCK ; end

    def ftype
      case (@__mode || __uninitialized) & S_IFMT
      when S_IFREG  then "file"
      when S_IFDIR  then "directory"
      when S_IFCHR  then "characterSpecial"
      when S_IFBLK  then "blockSpecial"
      when S_IFIFO  then "fifo"
      when S_IFLNK  then "link"
      when S_IFSOCK then "socket"
      else "unknown"
      end
    end

    def setuid?  ; ((@__mode || __uninitialized) & 0o4000) != 0 ; end
    def setgid?  ; ((@__mode || __uninitialized) & 0o2000) != 0 ; end
    def sticky?  ; ((@__mode || __uninitialized) & 0o1000) != 0 ; end

    def world_readable?
      ((@__mode || __uninitialized) & 0o004) != 0 ? ((@__mode || __uninitialized) & 0o777) : nil
    end

    def world_writable?
      ((@__mode || __uninitialized) & 0o002) != 0 ? ((@__mode || __uninitialized) & 0o777) : nil
    end

    def zero?     ; (@__size || __uninitialized) == 0 ; end
    def size?     ; (@__size || __uninitialized) == 0 ? nil : (@__size || __uninitialized) ; end

    def owned?    ; (@__uid || __uninitialized) == Process.euid ; end
    def grpowned? ; (@__gid || __uninitialized) == Process.egid ; end

    def readable?       ; _access_ok?(Process.euid, Process.egid, 4) ; end
    def readable_real?  ; _access_ok?(Process.uid, Process.gid, 4) ; end
    def writable?       ; _access_ok?(Process.euid, Process.egid, 2) ; end
    def writable_real?  ; _access_ok?(Process.uid, Process.gid, 2) ; end
    def executable?     ; _access_ok?(Process.euid, Process.egid, 1) ; end
    def executable_real? ; _access_ok?(Process.uid, Process.gid, 1) ; end

    # access(2)-style permission check against this stat's mode bits.
    # The superuser reads/writes anything and executes whenever any x
    # bit is set (CRuby's eaccess semantics).
    private def _access_ok?(uid, gid, bit)
      __uninitialized if @__mode.nil?
      if uid == 0
        return bit != 1 || ((@__mode || __uninitialized) & 0o111) != 0
      end
      shift = (@__uid || __uninitialized) == uid ? 6 : ((@__gid || __uninitialized) == gid ? 3 : 0)
      (((@__mode || __uninitialized) >> shift) & bit) != 0
    end

    def dev_major  ; (@__dev || __uninitialized) >> 8 ; end
    def dev_minor  ; (@__dev || __uninitialized) & 0xff ; end
    def rdev_major ; (@__rdev || __uninitialized) >> 8 ; end
    def rdev_minor ; (@__rdev || __uninitialized) & 0xff ; end

    def birthtime
      __uninitialized if @__dev.nil?
      raise NotImplementedError, "birthtime() function is unimplemented"
    end

    def <=>(other)
      return nil unless other.is_a?(File::Stat)
      (@__mtime || __uninitialized) <=> other.mtime
    end

    # A Stat that `allocate` made and no `initialize` filled in raises
    # TypeError from every reader, as CRuby's `get_stat` does (#2006).
    private def __uninitialized
      raise TypeError, "uninitialized File::Stat"
    end

    def inspect
      return "#<File::Stat: uninitialized>" if @__dev.nil?
      # Times use Time#inspect (fractional seconds included), like CRuby.
      __sprintf_repr(
        "#<File::Stat dev=0x#{@__dev.to_s(16)}, ino=#{@__ino}, mode=0#{@__mode.to_s(8)}, " \
        "nlink=#{@__nlink}, uid=#{@__uid}, gid=#{@__gid}, rdev=0x#{@__rdev.to_s(16)}, " \
        "size=#{@__size}, blksize=#{@__blksize}, blocks=#{@__blocks}, " \
        "atime=#{@__atime.inspect}, mtime=#{@__mtime.inspect}, ctime=#{@__ctime.inspect}>"
      )
    end
  end
end
