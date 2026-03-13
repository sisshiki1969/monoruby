# frozen_string_literal: true
#
# ActiveSupport::Configurable for monoruby.
# Provides a configuration DSL for modules and classes.
#

module ActiveSupport
  module Configurable
    def self.included(base)
      base.extend(ClassMethods)
    end

    class Configuration
      def initialize
        @options = {}
      end

      def method_missing(method_name, *args, &block)
        name = method_name.to_s
        if name.end_with?("=")
          @options[name[0...-1]] = args[0]
        elsif @options.key?(name)
          @options[name]
        else
          @options[name] = nil
        end
      end

      def respond_to_missing?(method_name, include_private = false)
        true
      end

      def [](key)
        @options[key.to_s]
      end

      def []=(key, value)
        @options[key.to_s] = value
      end

      def key?(key)
        @options.key?(key.to_s)
      end

      def to_h
        @options.dup
      end
    end

    module ClassMethods
      def config
        @_config ||= Configuration.new
      end

      def configure
        yield config
      end

      def config_accessor(*names, instance_reader: true, instance_writer: true)
        names.each do |name|
          # Class-level
          define_method(name) do
            self.class.config[name]
          end if instance_reader

          define_method("#{name}=") do |val|
            self.class.config[name] = val
          end if instance_writer

          # Also define on singleton
          singleton_class.define_method(name) do
            config[name]
          end

          singleton_class.define_method("#{name}=") do |val|
            config[name] = val
          end
        end
      end
    end

    def config
      self.class.config
    end
  end
end
