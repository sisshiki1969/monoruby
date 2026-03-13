# frozen_string_literal: true
#
# ActiveSupport for monoruby.
# Main entry point that sets up the module and loads core components.
#

module ActiveSupport
  class << self
    def gem_version
      "7.0.0"
    end
    alias version gem_version
  end
end

require 'active_support/lazy_load_hooks'
require 'active_support/inflector'
require 'active_support/concern'
require 'active_support/autoload'
require 'active_support/core_ext/object'
require 'active_support/core_ext/string'
require 'active_support/core_ext/hash'
require 'active_support/core_ext/array'
require 'active_support/core_ext/module'
require 'active_support/core_ext/class'
require 'active_support/core_ext/integer'
require 'active_support/core_ext/regexp'
require 'active_support/hash_with_indifferent_access'
require 'active_support/callbacks'
require 'active_support/configurable'
require 'active_support/notifications'
require 'active_support/descendants_tracker'
