# frozen_string_literal: true
#
# ActiveModel - A minimal implementation of Rails' ActiveModel for monoruby.
#
# Provides model attributes, validations, callbacks, dirty tracking,
# naming, conversion, and a convenience Model module.

module ActiveModel
end

require 'active_model/errors'
require 'active_model/attributes'
require 'active_model/validations'
require 'active_model/callbacks'
require 'active_model/dirty'
require 'active_model/naming'
require 'active_model/conversion'
require 'active_model/model'
