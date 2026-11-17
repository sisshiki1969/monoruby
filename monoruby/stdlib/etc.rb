# frozen_string_literal: true
#
# Etc is CRuby's ext/etc/etc.c, built against monoruby's C API layer
# (monoruby_capi/ext/etc). This file sits in the stub tree so that
# `require "etc"` reaches the extension monoruby ships before a host
# CRuby's etc gem, whose etc.so cannot be loaded here.
require "etc.so"
