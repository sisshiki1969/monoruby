# frozen_string_literal: true
#
# ActiveSupport core extensions for Class.
#

require 'active_support/core_ext/module'

class Class
  # Defines a class-level attribute that is inherited by subclasses.
  # Each subclass gets its own copy when set.
  def class_attribute(*attrs, instance_accessor: true, instance_reader: true,
                      instance_writer: true, instance_predicate: true, default: nil)
    attrs.each do |name|
      # Use a unique ivar name to store the value
      ivar = "@#{name}"

      # Define class-level reader with inheritance
      singleton_class.define_method(name) do
        if instance_variable_defined?(ivar)
          instance_variable_get(ivar)
        else
          # Walk up superclass chain
          sc = superclass
          while sc
            if sc.instance_variable_defined?(ivar)
              return sc.instance_variable_get(ivar)
            end
            sc = sc.respond_to?(:superclass) ? sc.superclass : nil
          end
          nil
        end
      end

      # Define class-level writer
      singleton_class.define_method("#{name}=") do |val|
        instance_variable_set(ivar, val)
      end

      # Define class-level predicate
      if instance_predicate
        singleton_class.define_method("#{name}?") do
          !!send(name)
        end
      end

      # Define instance-level reader
      if instance_accessor && instance_reader
        define_method(name) do
          self.class.send(name)
        end

        if instance_predicate
          define_method("#{name}?") do
            !!self.class.send(name)
          end
        end
      end

      # Define instance-level writer
      if instance_accessor && instance_writer
        define_method("#{name}=") do |val|
          # Set on the singleton class of this instance's class?
          # Actually, class_attribute instance writer sets it on the instance
          instance_variable_set(ivar, val)
          # Also override the reader for this instance
          singleton_class = (class << self; self; end)
          local_ivar = ivar
          singleton_class.define_method(name) do
            instance_variable_get(local_ivar)
          end
          val
        end
      end

      # Set default value
      unless default.nil?
        send("#{name}=", default)
      end
    end
  end

  # cattr_accessor is an alias for mattr_accessor on classes
  def cattr_accessor(*syms, **options)
    mattr_accessor(*syms, **options)
  end

  def cattr_reader(*syms, **options)
    mattr_reader(*syms, **options)
  end

  def cattr_writer(*syms, **options)
    mattr_writer(*syms, **options)
  end
end
