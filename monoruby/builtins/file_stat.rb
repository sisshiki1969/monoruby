class File
  # File::Stat — the fields (@dev, @ino, @mode, @nlink, @uid, @gid,
  # @rdev, @size, @blksize, @blocks, @atime, @mtime, @ctime) are
  # populated from a real stat(2)/lstat(2) by the Rust constructor
  # (`File.stat`, `File.lstat`, `File::Stat.new`). The accessors and
  # mode-bit predicates below read those fields.
  class Stat
    include Comparable

    attr_reader :dev, :ino, :mode, :nlink, :uid, :gid, :rdev,
                :size, :blksize, :blocks, :atime, :mtime, :ctime

    S_IFMT   = 0o170000
    S_IFSOCK = 0o140000
    S_IFLNK  = 0o120000
    S_IFREG  = 0o100000
    S_IFBLK  = 0o060000
    S_IFDIR  = 0o040000
    S_IFCHR  = 0o020000
    S_IFIFO  = 0o010000

    def directory? ; (@mode & S_IFMT) == S_IFDIR  ; end
    def file?      ; (@mode & S_IFMT) == S_IFREG  ; end
    def symlink?   ; (@mode & S_IFMT) == S_IFLNK  ; end
    def blockdev?  ; (@mode & S_IFMT) == S_IFBLK  ; end
    def chardev?   ; (@mode & S_IFMT) == S_IFCHR  ; end
    def pipe?      ; (@mode & S_IFMT) == S_IFIFO  ; end
    def socket?    ; (@mode & S_IFMT) == S_IFSOCK ; end

    def ftype
      case @mode & S_IFMT
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

    def setuid?  ; (@mode & 0o4000) != 0 ; end
    def setgid?  ; (@mode & 0o2000) != 0 ; end
    def sticky?  ; (@mode & 0o1000) != 0 ; end

    def world_readable?
      (@mode & 0o004) != 0 ? (@mode & 0o777) : nil
    end

    def world_writable?
      (@mode & 0o002) != 0 ? (@mode & 0o777) : nil
    end

    def zero?     ; @size == 0 ; end
    def size?     ; @size == 0 ? nil : @size ; end

    def owned?    ; @uid == Process.euid ; end
    def grpowned? ; @gid == Process.egid ; end

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
      if uid == 0
        return bit != 1 || (@mode & 0o111) != 0
      end
      shift = @uid == uid ? 6 : (@gid == gid ? 3 : 0)
      ((@mode >> shift) & bit) != 0
    end

    def dev_major  ; @dev >> 8 ; end
    def dev_minor  ; @dev & 0xff ; end
    def rdev_major ; @rdev >> 8 ; end
    def rdev_minor ; @rdev & 0xff ; end

    def birthtime
      raise NotImplementedError, "birthtime() function is unimplemented"
    end

    def <=>(other)
      return nil unless other.is_a?(File::Stat)
      @mtime <=> other.mtime
    end

    def inspect
      # Times use Time#inspect (fractional seconds included), like CRuby.
      "#<File::Stat dev=0x#{@dev.to_s(16)}, ino=#{@ino}, mode=0#{@mode.to_s(8)}, " \
      "nlink=#{@nlink}, uid=#{@uid}, gid=#{@gid}, rdev=0x#{@rdev.to_s(16)}, " \
      "size=#{@size}, blksize=#{@blksize}, blocks=#{@blocks}, " \
      "atime=#{@atime.inspect}, mtime=#{@mtime.inspect}, ctime=#{@ctime.inspect}>"
    end
  end
end
