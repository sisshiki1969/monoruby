class Errno
  class ENOENT < SystemCallError
    Errno = 2
    def initialize(msg = nil)
      super(msg ? "No such file or directory - #{msg}" : "No such file or directory")
    end
  end

  class EEXIST < SystemCallError
    Errno = 17
    def initialize(msg = nil)
      super(msg ? "File exists - #{msg}" : "File exists")
    end
  end

  class EACCES < SystemCallError
    Errno = 13
    def initialize(msg = nil)
      super(msg ? "Permission denied - #{msg}" : "Permission denied")
    end
  end

  class EPERM < SystemCallError
    Errno = 1
  end

  class ESRCH < SystemCallError
    Errno = 3
  end

  class EINTR < SystemCallError
    Errno = 4
  end

  class EIO < SystemCallError
    Errno = 5
  end

  class ENXIO < SystemCallError
    Errno = 6
  end

  class E2BIG < SystemCallError
    Errno = 7
  end

  class ENOEXEC < SystemCallError
    Errno = 8
  end

  class EBADF < SystemCallError
    Errno = 9
  end

  class ECHILD < SystemCallError
    Errno = 10
  end

  class EAGAIN < SystemCallError
    Errno = 11
  end

  class ENOMEM < SystemCallError
    Errno = 12
  end

  class EFAULT < SystemCallError
    Errno = 14
  end

  class EBUSY < SystemCallError
    Errno = 16
  end

  class EXDEV < SystemCallError
    Errno = 18
  end

  class ENODEV < SystemCallError
    Errno = 19
  end

  class ENOTDIR < SystemCallError
    Errno = 20
  end

  class EISDIR < SystemCallError
    Errno = 21
  end

  class EINVAL < SystemCallError
    Errno = 22
  end

  class ENFILE < SystemCallError
    Errno = 23
  end

  class EMFILE < SystemCallError
    Errno = 24
  end

  class ENOTTY < SystemCallError
    Errno = 25
  end

  class EFBIG < SystemCallError
    Errno = 27
  end

  class ENOSPC < SystemCallError
    Errno = 28
  end

  class ESPIPE < SystemCallError
    Errno = 29
  end

  class EROFS < SystemCallError
    Errno = 30
  end

  class EMLINK < SystemCallError
    Errno = 31
  end

  class EPIPE < SystemCallError
    Errno = 32
  end

  class EDOM < SystemCallError
    Errno = 33
  end

  class ERANGE < SystemCallError
    Errno = 34
  end

  class EDEADLK < SystemCallError
    Errno = 35
  end

  class ENAMETOOLONG < SystemCallError
    Errno = 36
  end

  class ENOLCK < SystemCallError
    Errno = 37
  end

  class ENOSYS < SystemCallError
    Errno = 38
  end

  class ENOTEMPTY < SystemCallError
    Errno = 39
    def initialize(msg = nil)
      super(msg ? "Directory not empty - #{msg}" : "Directory not empty")
    end
  end

  class ELOOP < SystemCallError
    Errno = 40
  end

  class EPROTO < SystemCallError
    Errno = 71
  end

  class ENOTSUP < SystemCallError
    Errno = 95
  end

  class EADDRINUSE < SystemCallError
    Errno = 98
  end

  class EADDRNOTAVAIL < SystemCallError
    Errno = 99
  end

  class ENETUNREACH < SystemCallError
    Errno = 101
  end

  class ECONNABORTED < SystemCallError
    Errno = 103
  end

  class ECONNRESET < SystemCallError
    Errno = 104
    def initialize(msg = nil)
      super(msg ? "Connection reset by peer - #{msg}" : "Connection reset by peer")
    end
  end

  class ENOBUFS < SystemCallError
    Errno = 105
  end

  class EISCONN < SystemCallError
    Errno = 106
  end

  class ENOTCONN < SystemCallError
    Errno = 107
  end

  class ETIMEDOUT < SystemCallError
    Errno = 110
    def initialize(msg = nil)
      super(msg ? "Connection timed out - #{msg}" : "Connection timed out")
    end
  end

  class ECONNREFUSED < SystemCallError
    Errno = 111
    def initialize(msg = nil)
      super(msg ? "Connection refused - #{msg}" : "Connection refused")
    end
  end

  class EHOSTUNREACH < SystemCallError
    Errno = 113
  end

  class EALREADY < SystemCallError
    Errno = 114
  end

  class EINPROGRESS < SystemCallError
    Errno = 115
  end

  class EISDIR < SystemCallError
    def initialize(msg = nil)
      super(msg ? "Is a directory - #{msg}" : "Is a directory")
    end
  end

  class ENOTDIR < SystemCallError
    def initialize(msg = nil)
      super(msg ? "Not a directory - #{msg}" : "Not a directory")
    end
  end

  class EPERM < SystemCallError
    def initialize(msg = nil)
      super(msg ? "Operation not permitted - #{msg}" : "Operation not permitted")
    end
  end

  class EINVAL < SystemCallError
    def initialize(msg = nil)
      super(msg ? "Invalid argument - #{msg}" : "Invalid argument")
    end
  end

  class EIO < SystemCallError
    def initialize(msg = nil)
      super(msg ? "Input/output error - #{msg}" : "Input/output error")
    end
  end

  class EAGAIN < SystemCallError
    def initialize(msg = nil)
      super(msg ? "Resource temporarily unavailable - #{msg}" : "Resource temporarily unavailable")
    end
  end

  class EPIPE < SystemCallError
    def initialize(msg = nil)
      super(msg ? "Broken pipe - #{msg}" : "Broken pipe")
    end
  end

  class EBADF < SystemCallError
    def initialize(msg = nil)
      super(msg ? "Bad file descriptor - #{msg}" : "Bad file descriptor")
    end
  end

  class ENOMEM < SystemCallError
    def initialize(msg = nil)
      super(msg ? "Cannot allocate memory - #{msg}" : "Cannot allocate memory")
    end
  end

  class EBUSY < SystemCallError
    def initialize(msg = nil)
      super(msg ? "Device or resource busy - #{msg}" : "Device or resource busy")
    end
  end

  class EROFS < SystemCallError
    def initialize(msg = nil)
      super(msg ? "Read-only file system - #{msg}" : "Read-only file system")
    end
  end

  class ENOSPC < SystemCallError
    def initialize(msg = nil)
      super(msg ? "No space left on device - #{msg}" : "No space left on device")
    end
  end

  class EADDRINUSE < SystemCallError
    def initialize(msg = nil)
      super(msg ? "Address already in use - #{msg}" : "Address already in use")
    end
  end
end

class SignalException
  def initialize(sig = nil)
    if sig.is_a?(Integer)
      super("Signal #{sig}")
    elsif sig
      super(sig.to_s)
    else
      super("SignalException")
    end
  end
end

class Interrupt < SignalException
  def initialize(msg = nil)
    if msg
      super(msg)
    else
      super("Interrupt")
    end
  end
end