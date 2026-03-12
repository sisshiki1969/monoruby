# frozen_string_literal: true
#
# ActiveModel::Model - Convenience module for plain-Ruby objects that need
# to interact with Action Pack and Action View.
#
# Includes Validations, Conversion, Naming and hash-based initialization.

require 'active_model/validations'
require 'active_model/conversion'
require 'active_model/naming'

module ActiveModel
  module Model
    def self.included(base)
      base.include ActiveModel::Validations
      base.include ActiveModel::Conversion
      base.include ActiveModel::Naming
      base.extend ClassMethods
    end

    module ClassMethods
      # No-op for compatibility; subclasses may override
    end

    # Initialize with a hash of attributes
    def initialize(attributes = {})
      assign_attributes(attributes) if attributes
    end

    def assign_attributes(new_attributes)
      return unless new_attributes

      new_attributes.each do |key, value|
        setter = :"#{key}="
        if respond_to?(setter)
          send(setter, value)
        else
          raise "unknown attribute '#{key}' for #{self.class.name}"
        end
      end
    end
    alias attributes= assign_attributes

    def persisted?
      false
    end

    def new_record?
      true
    end
  end
end
