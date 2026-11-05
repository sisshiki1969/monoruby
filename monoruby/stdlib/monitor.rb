# frozen_string_literal: true

# `require "monitor"`. Monitor, MonitorMixin and MonitorMixin::ConditionVariable
# are part of the bootstrap (builtins/monitor.rb, loaded by startup.rb), so
# this file only has to exist: it keeps `require "monitor"` from reaching the
# host CRuby's monitor.rb, which needs its C extension (monitor.so).
#
# It must not define them a second time. Methods defined here would report
# no `source_location` (a path under the install root reads as internal),
# where the bootstrap's report `<internal:monitor>`. redis 4.8 takes
# `File.expand_path("..", MonitorMixin.instance_method(:synchronize)
# .source_location.first)` at load time and fails on nil.
