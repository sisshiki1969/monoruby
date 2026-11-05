# frozen_string_literal: true
#
# Zlib is CRuby's ext/zlib/zlib.c, built against monoruby's C API layer
# (monoruby_capi/ext/zlib) and linked with zlib itself. This file sits in
# the stub tree so that `require "zlib"` reaches the extension monoruby
# ships before a host CRuby's zlib gem, whose zlib.so cannot be loaded
# here.
require "zlib.so"
