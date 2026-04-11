# Fiber stub for monoruby.
#
# In Ruby 4.0 Fiber is built-in, so `require "fiber"` is effectively a
# no-op. This file exists solely so that gems that still emit
# `require "fiber"` continue to load.
