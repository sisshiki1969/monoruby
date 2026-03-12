# frozen_string_literal: true
#
# ActiveSupport core extensions for Module.
#

class Module
  # Delegate methods to another object
  def delegate(*methods, to: nil, prefix: nil, allow_nil: false, private: false)
    unless to
      raise ArgumentError, "Delegation needs a target. Supply a keyword argument 'to'"
    end

    methods.each do |method|
      method_name = method.to_s

      # Build the delegated method name
      if prefix == true
        delegated_name = "#{to}_#{method_name}"
      elsif prefix
        delegated_name = "#{prefix}_#{method_name}"
      else
        delegated_name = method_name
      end

      # Define the delegation method
      if allow_nil
        define_method(delegated_name) do |*args, &block|
          target = if to.is_a?(Symbol) || to.is_a?(String)
                     if to.to_s == "class"
                       self.class
                     else
                       send(to)
                     end
                   else
                     to
                   end
          if target.nil?
            nil
          else
            target.public_send(method, *args, &block)
          end
        end
      else
        define_method(delegated_name) do |*args, &block|
          target = if to.is_a?(Symbol) || to.is_a?(String)
                     if to.to_s == "class"
                       self.class
                     else
                       send(to)
                     end
                   else
                     to
                   end
          target.public_send(method, *args, &block)
        end
      end

      if private
        private delegated_name
      end
    end
  end

  # Delegate missing methods to another object
  def delegate_missing_to(target)
    define_method(:method_missing) do |method_name, *args, &block|
      t = if target.is_a?(Symbol) || target.is_a?(String)
            send(target)
          else
            target
          end
      if t.respond_to?(method_name)
        t.public_send(method_name, *args, &block)
      else
        super(method_name, *args, &block)
      end
    end

    define_method(:respond_to_missing?) do |method_name, include_private = false|
      t = if target.is_a?(Symbol) || target.is_a?(String)
            send(target)
          else
            target
          end
      t.respond_to?(method_name) || super(method_name, include_private)
    end
  end

  # Module-level accessor (like attr_accessor but for module/class level)
  def mattr_accessor(*syms, instance_writer: true, instance_reader: true, instance_accessor: true, default: nil)
    mattr_reader(*syms, instance_reader: instance_reader, instance_accessor: instance_accessor, default: default)
    mattr_writer(*syms, instance_writer: instance_writer, instance_accessor: instance_accessor, default: default)
  end

  def mattr_reader(*syms, instance_reader: true, instance_accessor: true, default: nil)
    syms.each do |sym|
      raise NameError, "invalid attribute name: #{sym}" unless sym.to_s =~ /\A[_A-Za-z]\w*\z/

      # Class-level reader
      class_eval <<-RUBY
        def self.#{sym}
          @#{sym}
        end
      RUBY

      # Instance-level reader
      if instance_reader && instance_accessor
        class_eval <<-RUBY
          def #{sym}
            self.class.#{sym}
          end
        RUBY
      end

      # Set default
      instance_variable_set("@#{sym}", default) unless default.nil?
    end
  end

  def mattr_writer(*syms, instance_writer: true, instance_accessor: true, default: nil)
    syms.each do |sym|
      raise NameError, "invalid attribute name: #{sym}" unless sym.to_s =~ /\A[_A-Za-z]\w*\z/

      # Class-level writer
      class_eval <<-RUBY
        def self.#{sym}=(val)
          @#{sym} = val
        end
      RUBY

      # Instance-level writer
      if instance_writer && instance_accessor
        class_eval <<-RUBY
          def #{sym}=(val)
            self.class.#{sym} = val
          end
        RUBY
      end

      # Set default
      instance_variable_set("@#{sym}", default) unless default.nil?
    end
  end
end
