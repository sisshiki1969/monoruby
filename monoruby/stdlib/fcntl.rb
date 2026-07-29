# frozen_string_literal: true
#
# `fcntl` stub for monoruby.
#
# CRuby ships this as a C extension that only defines the Fcntl constant
# module; the actual work happens through `IO#fcntl` (a monoruby
# builtin). The fcntl command numbers and open flags differ between
# Linux and Darwin, so branch on the runtime platform (monoruby runs on
# both). `O_NONBLOCK` in particular is 0o4000 on Linux but 0x0004 on
# Darwin — using the wrong bit makes `io/nonblock` silently ineffective.
module Fcntl
  F_DUPFD  = 0
  F_GETFD  = 1
  F_SETFD  = 2
  F_GETFL  = 3
  F_SETFL  = 4

  FD_CLOEXEC = 1

  O_RDONLY   = 0
  O_WRONLY   = 1
  O_RDWR     = 2
  O_ACCMODE  = 3

  if RUBY_PLATFORM.include?('darwin')
    F_GETLK  = 7
    F_SETLK  = 8
    F_SETLKW = 9

    F_RDLCK = 1
    F_WRLCK = 3
    F_UNLCK = 2

    O_NONBLOCK = 0x0004
    O_APPEND   = 0x0008
    O_CREAT    = 0x0200
    O_TRUNC    = 0x0400
    O_EXCL     = 0x0800
    O_NOCTTY   = 0x20000

    F_DUPFD_CLOEXEC = 67
  else
    F_GETLK  = 5
    F_SETLK  = 6
    F_SETLKW = 7

    F_RDLCK = 0
    F_WRLCK = 1
    F_UNLCK = 2

    O_NONBLOCK = 0o4000
    O_APPEND   = 0o2000
    O_CREAT    = 0o100
    O_TRUNC    = 0o1000
    O_EXCL     = 0o200
    O_NOCTTY   = 0o400

    F_DUPFD_CLOEXEC = 1030
  end
  O_NDELAY = O_NONBLOCK

  VERSION = "1.2.0"
end
