# frozen_string_literal: true
#
# Thread-free Open3 for monoruby.
#
# CRuby's Open3.capture* drain the child's stdout and stderr in separate
# threads so neither pipe can fill its buffer (~64KB) and block the child
# while the parent is busy reading the other stream. monoruby's Thread is
# cooperative — a `Thread.new { ... }` block does not run concurrently, it
# runs when `#value`/`#join` is called — so that pattern deadlocks once a
# child emits more than a pipe buffer on one stream.
#
# This implementation multiplexes all the pipes with IO.select instead, so
# no real threads are needed. `spawn` and `Process.detach` are provided
# natively (see the process builtin / startup.rb).

module Open3
  def popen3(*cmd, &block)
    opts = (Hash === cmd.last) ? cmd.pop.dup : {}
    in_r, in_w = IO.pipe
    out_r, out_w = IO.pipe
    err_r, err_w = IO.pipe
    opts[:in] = in_r
    opts[:out] = out_w
    opts[:err] = err_w
    in_w.sync = true
    popen_run(cmd, opts, [in_r, out_w, err_w], [in_w, out_r, err_r], &block)
  end
  module_function :popen3

  def popen2(*cmd, &block)
    opts = (Hash === cmd.last) ? cmd.pop.dup : {}
    in_r, in_w = IO.pipe
    out_r, out_w = IO.pipe
    opts[:in] = in_r
    opts[:out] = out_w
    in_w.sync = true
    popen_run(cmd, opts, [in_r, out_w], [in_w, out_r], &block)
  end
  module_function :popen2

  def popen2e(*cmd, &block)
    opts = (Hash === cmd.last) ? cmd.pop.dup : {}
    in_r, in_w = IO.pipe
    out_r, out_w = IO.pipe
    opts[:in] = in_r
    # Send both stdout and stderr of the child to the same pipe.
    opts[:out] = out_w
    opts[:err] = out_w
    in_w.sync = true
    popen_run(cmd, opts, [in_r, out_w], [in_w, out_r], &block)
  end
  module_function :popen2e

  def popen_run(cmd, opts, child_io, parent_io)
    pid = spawn(*cmd, opts)
    wait_thr = Process.detach(pid)
    child_io.each { |io| io.close unless io.closed? }
    result = [*parent_io, wait_thr]
    if defined?(yield)
      begin
        return yield(*result)
      ensure
        parent_io.each { |io| io.close unless io.closed? }
        wait_thr.join
      end
    end
    result
  end
  module_function :popen_run

  def capture3(*cmd)
    opts = (Hash === cmd.last) ? cmd.pop.dup : {}
    stdin_data = opts.delete(:stdin_data) || ''
    binmode = opts.delete(:binmode)
    popen3(*cmd, opts) do |i, o, e, t|
      if binmode
        i.binmode
        o.binmode
        e.binmode
      end
      bufs = __drain(i, stdin_data, [o, e])
      [bufs[o], bufs[e], t.value]
    end
  end
  module_function :capture3

  def capture2(*cmd)
    opts = (Hash === cmd.last) ? cmd.pop.dup : {}
    stdin_data = opts.delete(:stdin_data) || ''
    binmode = opts.delete(:binmode)
    popen2(*cmd, opts) do |i, o, t|
      if binmode
        i.binmode
        o.binmode
      end
      bufs = __drain(i, stdin_data, [o])
      [bufs[o], t.value]
    end
  end
  module_function :capture2

  def capture2e(*cmd)
    opts = (Hash === cmd.last) ? cmd.pop.dup : {}
    stdin_data = opts.delete(:stdin_data) || ''
    binmode = opts.delete(:binmode)
    popen2e(*cmd, opts) do |i, oe, t|
      if binmode
        i.binmode
        oe.binmode
      end
      bufs = __drain(i, stdin_data, [oe])
      [bufs[oe], t.value]
    end
  end
  module_function :capture2e

  # Feed `stdin_data` into `wr` while reading every IO in `readers` to EOF,
  # all multiplexed through a single IO.select loop so no pipe can deadlock.
  # Returns a Hash mapping each reader IO to its collected String.
  def __drain(wr, stdin_data, readers)
    chunks = {}
    readers.each { |io| chunks[io] = [] }
    pending = readers.dup
    data = (stdin_data || '').b
    if wr && data.empty?
      wr.close
      wr = nil
    end
    until pending.empty? && wr.nil?
      rs, ws, = IO.select(pending.empty? ? nil : pending, wr ? [wr] : nil)
      rs&.each do |io|
        begin
          chunks[io] << io.read_nonblock(65536)
        rescue EOFError
          pending.delete(io)
          io.close unless io.closed?
        rescue IO::WaitReadable
        end
      end
      if ws && !ws.empty? && wr
        begin
          n = wr.write_nonblock(data)
          data = data.byteslice(n..-1) || ''.b
          if data.empty?
            wr.close
            wr = nil
          end
        rescue IO::WaitWritable
        rescue Errno::EPIPE
          wr.close unless wr.closed?
          wr = nil
        end
      end
    end
    result = {}
    chunks.each { |io, parts| result[io] = parts.join }
    result
  end
  module_function :__drain
end
