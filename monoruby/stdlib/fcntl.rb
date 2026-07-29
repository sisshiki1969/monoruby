# frozen_string_literal: true
#
# `fcntl` stub for monoruby.
#
# CRuby ships this as a C extension that only defines the Fcntl constant
# module; the actual work happens through `IO#fcntl` (a monoruby
# builtin). The values are the Linux/x86-64 ones, matching the vendored
# platform.
module Fcntl
  F_DUPFD  = 0
  F_GETFD  = 1
  F_SETFD  = 2
  F_GETFL  = 3
  F_SETFL  = 4
  F_GETLK  = 5
  F_SETLK  = 6
  F_SETLKW = 7

  FD_CLOEXEC = 1

  F_RDLCK = 0
  F_WRLCK = 1
  F_UNLCK = 2

  O_RDONLY   = 0
  O_WRONLY   = 1
  O_RDWR     = 2
  O_ACCMODE  = 3
  O_CREAT    = 64
  O_EXCL     = 128
  O_NOCTTY   = 256
  O_TRUNC    = 512
  O_APPEND   = 1024
  O_NONBLOCK = 2048
  O_NDELAY   = O_NONBLOCK

  F_DUPFD_CLOEXEC = 1030

  VERSION = "1.2.0"
end
