# `Errno::E*` exception classes and their `Errno` constants are populated
# by Rust-side `errno::init` (see monoruby/src/builtins/errno.rs), which
# walks the host's `libc::E*` table at startup so any errno present on the
# system (`Errno::ENETDOWN`, `Errno::EPROTOTYPE`, …) is reachable.
# We only attach a per-class `initialize` here so `Errno::E*.new(arg)`
# yields CRuby's "<strerror> - <arg>" message format.
class Errno
  class ENOENT < SystemCallError
    def initialize(msg = nil)
      super(msg ? "No such file or directory - #{msg}" : "No such file or directory")
    end
  end

  class EEXIST < SystemCallError
    def initialize(msg = nil)
      super(msg ? "File exists - #{msg}" : "File exists")
    end
  end

  class EACCES < SystemCallError
    def initialize(msg = nil)
      super(msg ? "Permission denied - #{msg}" : "Permission denied")
    end
  end

  class ENOTEMPTY < SystemCallError
    def initialize(msg = nil)
      super(msg ? "Directory not empty - #{msg}" : "Directory not empty")
    end
  end

  class ECONNRESET < SystemCallError
    def initialize(msg = nil)
      super(msg ? "Connection reset by peer - #{msg}" : "Connection reset by peer")
    end
  end

  class ETIMEDOUT < SystemCallError
    def initialize(msg = nil)
      super(msg ? "Connection timed out - #{msg}" : "Connection timed out")
    end
  end

  class ECONNREFUSED < SystemCallError
    def initialize(msg = nil)
      super(msg ? "Connection refused - #{msg}" : "Connection refused")
    end
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