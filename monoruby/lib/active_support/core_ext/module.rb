# frozen_string_literal: true
#
# ActiveSupport core extensions for Module.
#
# NOTE: monoruby doesn't have `public_send` or reliable `define_method` closures.
# We use `class_eval` with string evaluation and `send` as workarounds.
#

class Module
  # Delegate methods to another object
  def delegate(*methods, to: nil, prefix: nil, allow_nil: false, private: false)
    unless to
      raise ArgumentError, "Delegation needs a target. Supply a keyword argument 'to'"
    end

    to_s_str = to.to_s

    # Determine how to access the target
    if to_s_str == "class"
      target_expr = "self.class"
    elsif to_s_str.start_with?("@")
      target_expr = "instance_variable_get(\"#{to_s_str}\")"
    else
      target_expr = "send(:#{to_s_str})"
    end

    methods.each do |method|
      method_name = method.to_s

      # Build the delegated method name
      if prefix == true
        delegated_name = "#{to_s_str.sub(/^@/, '')}_#{method_name}"
      elsif prefix
        delegated_name = "#{prefix}_#{method_name}"
      else
        delegated_name = method_name
      end

      if allow_nil
        class_eval(
          "def #{delegated_name}(*args, &block)\n" \
          "  _target_ = #{target_expr}\n" \
          "  return nil if _target_.nil?\n" \
          "  _target_.send(:#{method_name}, *args, &block)\n" \
          "end"
        )
      else
        class_eval(
          "def #{delegated_name}(*args, &block)\n" \
          "  #{target_expr}.send(:#{method_name}, *args, &block)\n" \
          "end"
        )
      end

      if private
        private delegated_name.to_sym
      end
    end
  end

  # Delegate missing methods to another object
  def delegate_missing_to(target_name)
    target_str = target_name.to_s

    if target_str.start_with?("@")
      target_expr = "instance_variable_get(\"#{target_str}\")"
    else
      target_expr = "send(:#{target_str})"
    end

    class_eval(
      "def method_missing(method_name, *args, &block)\n" \
      "  _t_ = #{target_expr}\n" \
      "  if _t_.respond_to?(method_name)\n" \
      "    _t_.send(method_name, *args, &block)\n" \
      "  else\n" \
      "    super\n" \
      "  end\n" \
      "end"
    )

    class_eval(
      "def respond_to_missing?(method_name, include_private = false)\n" \
      "  _t_ = #{target_expr}\n" \
      "  _t_.respond_to?(method_name) || super\n" \
      "end"
    )
  end

  # Module-level accessor (like attr_accessor but for module/class level)
  def mattr_accessor(*syms, instance_writer: true, instance_reader: true, instance_accessor: true, default: nil)
    mattr_reader(*syms, instance_reader: instance_reader, instance_accessor: instance_accessor, default: default)
    mattr_writer(*syms, instance_writer: instance_writer, instance_accessor: instance_accessor, default: default)
  end

  def mattr_reader(*syms, instance_reader: true, instance_accessor: true, default: nil)
    syms.each do |sym|
      ivar = "@#{sym}"

      # Class-level reader
      class_eval("def self.#{sym}; #{ivar}; end")

      # Instance-level reader
      if instance_reader && instance_accessor
        class_eval("def #{sym}; self.class.#{sym}; end")
      end

      # Set default
      instance_variable_set(ivar, default) unless default.nil?
    end
  end

  def mattr_writer(*syms, instance_writer: true, instance_accessor: true, default: nil)
    syms.each do |sym|
      ivar = "@#{sym}"

      # Class-level writer
      class_eval("def self.#{sym}=(val); #{ivar} = val; end")

      # Instance-level writer
      if instance_writer && instance_accessor
        class_eval("def #{sym}=(val); self.class.#{sym} = val; end")
      end

      # Set default
      instance_variable_set(ivar, default) unless default.nil?
    end
  end
end
