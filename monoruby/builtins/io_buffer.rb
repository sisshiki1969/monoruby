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

    # write(io, length = nil, offset = 0) -> Integer
    #
    # Writes `length` bytes starting at `offset` (all of the rest when
    # `length` is nil or, before Ruby 4.1, 0) to `io` and returns how
    # many were written.
    def write(io, length = nil, offset = 0)
      unless io.is_a?(::IO)
        io = ::IO.try_convert(io) or
          Kernel.raise TypeError, "no implicit conversion of #{__builtin_class_name(io)} into IO"
      end
      offset = Integer(offset)
      Kernel.raise ArgumentError, "Offset can't be negative!" if offset < 0
      length = length.nil? ? 0 : Integer(length)
      Kernel.raise ArgumentError, "Length can't be negative!" if length < 0
      length = size - offset if length == 0
      if length < 0 || offset + length > size
        Kernel.raise ArgumentError, "Specified offset+length is bigger than the buffer size!"
      end
      return 0 if length == 0
      io.write(get_string(offset, length))
    end
  end
end
