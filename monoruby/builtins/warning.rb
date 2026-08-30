module Warning
  @categories = {
    deprecated: false, experimental: true, performance: false,
    strict_unused_block: false,
  }

  def self.categories
    @categories.keys
  end

  def self.[](category)
    unless category.is_a?(Symbol)
      raise TypeError, "wrong argument type #{category.class} (expected Symbol)"
    end
    unless @categories.key?(category)
      raise ArgumentError, "unknown category: #{category}"
    end
    @categories[category]
  end

  def self.[]=(category, value)
    unless category.is_a?(Symbol)
      raise TypeError, "wrong argument type #{category.class} (expected Symbol)"
    end
    unless @categories.key?(category)
      raise ArgumentError, "unknown category: #{category}"
    end
    @categories[category] = value ? true : false
  end

  # The default `Warning.warn`. A categorised warning is suppressed when
  # its category is switched off (`Warning[:deprecated] = false`), which
  # is what `rb_warning_s_warn` checks before writing anything; an
  # uncategorised one always prints.
  def warn(msg, category: nil)
    unless category.nil?
      unless category.is_a?(Symbol)
        raise TypeError, "wrong argument type #{category.class} (expected Symbol)"
      end
      unless Warning.categories.include?(category)
        raise ArgumentError, "unknown category: #{category}"
      end
      return nil unless Warning[category]
    end
    $stderr.write(msg)
    nil
  end
  extend self
end
