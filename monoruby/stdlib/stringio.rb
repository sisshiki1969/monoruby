# frozen_string_literal: true
#
# StringIO is CRuby's ext/stringio/stringio.c, built against monoruby's C
# API layer (monoruby_capi/ext/stringio). This file sits in the stub tree
# so that `require "stringio"` reaches the extension monoruby ships before
# a host CRuby's stringio gem, whose stringio.so cannot be loaded here.
require "stringio.so"
