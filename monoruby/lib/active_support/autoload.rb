# frozen_string_literal: true
#
# ActiveSupport::Autoload (simplified) for monoruby.
# Since monoruby is single-threaded, this is a simple eager/lazy load mechanism.
#

module ActiveSupport
  module Autoload
    def self.extended(base)
      base.instance_variable_set(:@_autoloads, {})
      base.instance_variable_set(:@_under_path, nil)
      base.instance_variable_set(:@_at_path, nil)
    end

    def autoload(const_name, path = nil)
      if path.nil?
        # Build path from module name and constant
        full_path = _autoload_path(const_name)
      else
        full_path = path
      end

      @_autoloads ||= {}
      @_autoloads[const_name] = full_path

      # Use Ruby's built-in autoload
      super(const_name, full_path)
    end

    def autoload_under(path)
      @_under_path = path
      yield
    ensure
      @_under_path = nil
    end

    def autoload_at(path)
      @_at_path = path
      yield
    ensure
      @_at_path = nil
    end

    def eager_load!
      @_autoloads ||= {}
      @_autoloads.each_value do |path|
        begin
          require path
        rescue LoadError
          # Skip missing files
        end
      end
    end

    private

    def _autoload_path(const_name)
      if @_at_path
        @_at_path
      else
        # Convert module path to file path
        parts = name.split("::")
        parts = parts.map { |p| _underscore(p) }
        if @_under_path
          parts << @_under_path
        end
        parts << _underscore(const_name.to_s)
        parts.join("/")
      end
    end

    def _underscore(str)
      word = str.dup
      word = word.gsub(/([A-Z\d]+)([A-Z][a-z])/) { $1 + "_" + $2 }
      word = word.gsub(/([a-z\d])([A-Z])/) { $1 + "_" + $2 }
      word.downcase
    end
  end
end
