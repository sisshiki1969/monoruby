# frozen_string_literal: true
#
# The strscan gem's own lib/strscan.rb (its `RUBY_ENGINE == "ruby"`
# branch): CRuby's ext/strscan/strscan.c, built against monoruby's C API
# layer (monoruby_capi/ext/strscan), plus the gem's Ruby half.
#
# This file sits in the stub tree so that `require "strscan"` reaches the
# extension monoruby ships before a host CRuby's strscan gem, whose
# strscan.so cannot be loaded here.
require "strscan.so"
require_relative "strscan/strscan"
