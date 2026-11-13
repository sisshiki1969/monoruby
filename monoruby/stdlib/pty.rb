# frozen_string_literal: true

# PTY — pseudo terminals, in Ruby over two primitives: `Process.__pty_open`
# (`posix_openpt` / `grantpt` / `unlockpt` and the slave opened by name)
# and `Process.spawn`'s private `__pty_session: slave_fd` option (the
# child calls `setsid`, takes the slave as its controlling terminal and
# on its stdin, stdout and stderr, then applies the other options).
# CRuby's ext/pty does the same around its own fork; its pty.so is built
# for CRuby, so without this file `require "pty"` would reach it and fail
# (shipit-engine requires it at boot).
module PTY
  # Raised by `PTY.check(pid, true)` when the child has stopped or exited.
  class ChildExited < RuntimeError
    # The child's Process::Status.
    attr_reader :status
  end

  class << self
    # PTY.open -> [master_io, slave_file]
    # PTY.open { |(master_io, slave_file)| ... } -> the block's value
    def open
      master_fd, slave_fd, name = Process.__pty_open
      master = IO.new(master_fd, "r+", path: "masterpty:#{name}")
      master.sync = true
      slave = File.new(slave_fd, "r+", path: name)
      slave.sync = true
      pair = [master, slave]
      return pair unless block_given?
      begin
        yield pair
      ensure
        pair.each { |io| io.close unless io.closed? }
      end
    end

    # PTY.spawn([env,] command_line [, options]) -> [r_pty, w_pty, pid]
    # PTY.spawn([env,] command, arg1, ... [, options]) { |r_pty, w_pty, pid| ... } -> nil
    #
    # Runs the command with a new pty's slave as its stdin, stdout and
    # stderr and as its controlling terminal; answers the master as a
    # reader and a duplicate writer. With no command the user's shell
    # runs, as for CRuby.
    def spawn(*args)
      args = [ENV["SHELL"] || "/bin/sh"] if args.empty? || (args.size == 1 && args[0].is_a?(Hash))
      opts = args.size > 1 && args.last.is_a?(Hash) ? args.pop.dup : {}
      master_fd, slave_fd, name = Process.__pty_open
      begin
        # The child puts the slave on its stdin, stdout and stderr before
        # it applies `opts`, so a redirect given here still wins (ext/pty).
        opts[:__pty_session] = slave_fd
        pid = Process.spawn(*args, opts)
      rescue Exception
        IO.new(master_fd).close
        raise
      ensure
        IO.new(slave_fd).close
      end
      r = File.new(master_fd, "r", path: name)
      w = File.new(r.fcntl(Fcntl::F_DUPFD_CLOEXEC, 0), "w", path: name)
      w.sync = true
      res = [r, w, pid]
      return res unless block_given?
      begin
        yield res
      ensure
        # CRuby's pty_detach_process.
        begin
          Process.detach(pid) if Process.waitpid(pid, Process::WNOHANG)
        rescue SystemCallError
        end
      end
      nil
    end
    alias getpty spawn

    # PTY.check(pid, raise = false) -> Process::Status or nil
    def check(pid, raise = false)
      begin
        cpid = Process.waitpid(pid, Process::WNOHANG | Process::WUNTRACED)
      rescue SystemCallError
        return nil
      end
      return nil unless cpid
      status = $?
      return status unless raise
      state =
        if status.stopped?
          "stopped"
        elsif (Process.kill(0, cpid) rescue false)
          "changed"
        else
          "exited"
        end
      exc = ChildExited.new("pty - #{state}: #{cpid}")
      exc.instance_variable_set(:@status, status)
      Kernel.raise exc
    end
  end
end

require "fcntl"
