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

  def warn(msg, category: nil)
    $stderr.write(msg)
    nil
  end
  extend self
end
