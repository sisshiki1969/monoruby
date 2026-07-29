# frozen_string_literal: true
#
# `io/nonblock` stub for monoruby: the O_NONBLOCK accessors CRuby ships
# as a C extension, expressed over the `IO#fcntl` builtin.
require 'fcntl'

class IO
  # Whether the underlying file descriptor has O_NONBLOCK set.
  def nonblock?
    (fcntl(Fcntl::F_GETFL) & Fcntl::O_NONBLOCK) != 0
  end

  # Set or clear O_NONBLOCK on the underlying file descriptor.
  def nonblock=(flag)
    flags = fcntl(Fcntl::F_GETFL)
    if flag
      fcntl(Fcntl::F_SETFL, flags | Fcntl::O_NONBLOCK)
    else
      fcntl(Fcntl::F_SETFL, flags & ~Fcntl::O_NONBLOCK)
    end
    flag
  end

  # With a block: set O_NONBLOCK to `nonblock` for the duration of the
  # block, restoring the previous state afterwards. Without a block:
  # set it and return self.
  def nonblock(nonblock = true)
    if block_given?
      prev = nonblock?
      begin
        self.nonblock = nonblock
        yield self
      ensure
        begin
          self.nonblock = prev
        rescue IOError, SystemCallError
          # The IO may have been closed inside the block.
        end
      end
    else
      self.nonblock = nonblock
      self
    end
  end
end
