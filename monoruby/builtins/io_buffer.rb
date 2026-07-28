class IO
  class Buffer
    include Comparable

    # Error hierarchy (CRuby io_buffer.c). The Buffer class itself is
    # native; only the exception constants live here.
    class AllocationError < RuntimeError; end
    class AccessError < RuntimeError; end
    class LockedError < RuntimeError; end
    class InvalidatedError < RuntimeError; end
    class MaskError < ArgumentError; end
  end
end
